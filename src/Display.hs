--port of https://github.com/bergey/haskell-OpenGL-examples

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Display where

import Control.Monad
import Data.Array.Unboxed
import Data.Bits
import Data.Word
import Metrics
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Exit
import Data.Dequeue
import Data.IORef
import Keys

import System.Exit (exitFailure)
import System.IO
import TIAColors
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW
-- import SDL.Vect

-- | Inform OpenGL about new pixel data.
updateTexture :: GL.TextureObject -> Ptr Word8 -> IO ()
updateTexture texName textureData = do
    GL.textureBinding GL.Texture2D $= Just texName
    GL.texSubImage2D
        GL.Texture2D
        0
        (GL.TexturePosition2D 0 0)
        (GL.TextureSize2D 64 96)
        (GL.PixelData GL.Red GL.UnsignedByte textureData)
    return ()

-- | Create OpenGL texture map to represent TV screen.
createImageTexture :: GL.TextureObject -> IO (Ptr Word8)
createImageTexture texName = do
    GL.textureBinding GL.Texture2D $= Just texName
--     print "createImageTexture"

    textureData <- mallocBytes 6144 :: IO (Ptr Word8)

    -- Allocate 6K of video RAM
    -- as width 64, height 96
    forM_ [0..6143] $ \i ->
        pokeElemOff textureData (fromIntegral $ i) 0

    GL.textureBinding GL.Texture2D $= Just texName
    GL.texImage2D
        GL.Texture2D
        GL.NoProxy
        0
        GL.R8
        (GL.TextureSize2D 64 96)
        0
        (GL.PixelData GL.Red GL.UnsignedByte textureData)

    GL.textureFilter   GL.Texture2D   $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)

    return textureData

createFontTexture :: GL.TextureObject -> Ptr Word8 -> IO ()
createFontTexture texName font_data = do
--     textureData2 <- mallocBytes (256*8*8) :: IO (Ptr Word8)
--     forM_ [0..255] $ \c ->
--         forM_ [0..8-1] $ \i ->
--             forM_ [0..8-1] $ \j -> do
--                 pokeElemOff textureData2 (fromIntegral $ c*8*8+i*8+j) (fromIntegral $ if c == 32 then 0 else 1)
--                 return ()

    GL.textureBinding GL.Texture2D $= Just texName

    GL.texImage2D
        GL.Texture2D
        GL.NoProxy
        0
        GL.R8
        (GL.TextureSize2D 256 96)
        0
        (GL.PixelData GL.Red GL.UnsignedByte font_data)

    GL.textureFilter   GL.Texture2D   $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)

--     forM_ [0..256*96-1] $ \i -> do
--         x <- peekElemOff font_data i
--         print (i, x)

-- | Compile and link vertex and fragment shaders.
createShaderProgram :: IO GL.Program
createShaderProgram = do
    -- compile vertex shader
    vs <- GL.createShader GL.VertexShader
    GL.shaderSourceBS vs $= vsSource
    GL.compileShader vs
    vsOK <- GL.get $ GL.compileStatus vs
    unless vsOK $ do
        hPutStrLn stderr "Error in vertex shader\n"
        exitFailure

    -- Do it again for the fragment shader
    fs <- GL.createShader GL.FragmentShader
    GL.shaderSourceBS fs $= fsSource
    GL.compileShader fs
    fsOK <- GL.get $ GL.compileStatus fs
    unless fsOK $ do
        hPutStrLn stderr "Error in fragment shader\n"
        msg <- GL.get $ GL.shaderInfoLog fs
        hPutStrLn stderr msg
        exitFailure

    program <- GL.createProgram
    GL.attachShader program vs
    GL.attachShader program fs
    GL.attribLocation program "coord2d" $= GL.AttribLocation 0
    GL.linkProgram program
    linkOK <- GL.get $ GL.linkStatus program

    unless linkOK $ do
        hPutStrLn stderr "GL.linkProgram error"
        plog <- GL.get $ GL.programInfoLog program
        putStrLn plog
        exitFailure

    return program

-- | Bind textures to appropriate locations in shader program.
connectProgramToTextures :: GL.Program -> Float -> GL.TextureObject -> GL.TextureObject -> GL.TextureObject -> IO ()
connectProgramToTextures program alpha current_frame_tex last_frame_tex font_tex = do
    GL.currentProgram $= Just program
    current_screen_tex_loc <- GL.uniformLocation program "current_frame"
    last_screen_tex_loc <- GL.uniformLocation program "last_frame"
    font_tex_loc <- GL.uniformLocation program "table"
    alpha_loc <- GL.uniformLocation program "alpha"
    print (current_screen_tex_loc, last_screen_tex_loc, font_tex_loc)

    GL.uniform alpha_loc $= alpha

    GL.activeTexture $= GL.TextureUnit 0
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just current_frame_tex

    GL.activeTexture $= GL.TextureUnit 1
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just font_tex

    GL.activeTexture $= GL.TextureUnit 2
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just last_frame_tex

    GL.uniform current_screen_tex_loc $= GL.Index1 (0::GL.GLint)
    GL.uniform font_tex_loc $= GL.Index1 (1::GL.GLint)
    GL.uniform last_screen_tex_loc $= GL.Index1 (2::GL.GLint)

    GL.validateProgram program
    status <- GL.get $ GL.validateStatus program
    unless status $ do
        hPutStrLn stderr "GL.linkProgram error"
        plog <- GL.get $ GL.programInfoLog program
        putStrLn plog
        exitFailure
    GL.currentProgram $= Just program

-- | Create all OpenGL objects required including shaders and textures.
initResources :: Float -> Ptr Word8 -> IO (GL.Program, GL.AttribLocation, GL.TextureObject, GL.TextureObject, Ptr Word8, Ptr Word8)
initResources alpha font_data = do
    [current_frame_tex, last_frame_tex, font_tex] <- GL.genObjectNames 3 :: IO [GL.TextureObject]

    textureData <- createImageTexture current_frame_tex
    lastTextureData <- createImageTexture last_frame_tex
    createFontTexture font_tex font_data

    program <- createShaderProgram
    connectProgramToTextures program alpha current_frame_tex last_frame_tex font_tex

    return (program, GL.AttribLocation 0, current_frame_tex, last_frame_tex, textureData, lastTextureData)

-- | Render VCS screen as pair of triangles.
draw :: Int -> Int -> GL.Program -> GL.AttribLocation -> IO ()
draw windowWidth windowHeight program attrib = do
--     print "Draw 1"
    GL.clearColor $= GL.Color4 0 0 0 0
    GL.clear [GL.ColorBuffer]
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral windowWidth) (fromIntegral windowHeight))

    GL.currentProgram $= Just program
    GL.vertexAttribArray attrib $= GL.Enabled
--     print "Draw 2"
    V.unsafeWith vertices $ \ptr ->
        GL.vertexAttribPointer attrib $=
          (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 ptr)
--     print "Draw 3"
    GL.drawArrays GL.Triangles 0 6
--     print "Draw 3"
    GL.vertexAttribArray attrib $= GL.Disabled
--     print "Draw done"
--     SDL.glSwapWindow window

vsSource, fsSource :: BS.ByteString
vsSource = BS.intercalate "\n"
           [
                "#version 110",
                "",
                "attribute vec2 position;",
                "varying vec2 texcoord;",
                "",
                "void main()",
                "{",
                "    gl_Position = vec4(position, 0.0, 1.0);",
                "    texcoord = vec2(0.5+0.5*position.x, 0.5-0.5*position.y);",
                "}"
           ]

fsSource = BS.intercalate "\n"
           [
                "#version 110",
                "",
                "uniform sampler2D current_frame;",
                "uniform sampler2D last_frame;",
                "uniform sampler2D table;",
                "uniform float alpha;",
                "varying vec2 texcoord;",
                "",
                "void main()",
                "{",
                "",
                "    vec4 current_index = texture2D(current_frame, texcoord);",

                "    int x = int(32.*8.*texcoord.x);",
                "    int y = int(16.*12.*texcoord.y);",

                "    int ix = x/8;",
                "    int iy = y/12;",
                "    int fx = x-8*ix;",
                "    int fy = y-12*iy;",

                "    int addr = 32*iy+ix;",
                "    int ty = addr/64;",
                "    int tx = addr-64*ty;",

                "    vec4 last_index = texture2D(current_frame, vec2(float(tx)/64., float(ty)/96.));",
--                 "    //float z = last_index.x>0.15 ? 0.0 : 1.0;//texture1D(table, last_index.x*8.*8.+float(ix)*8.+float(iy));",
--                 "    float z = float(tx);//last_index.x;",
--                 "    float z = float(iy)/16.0;//last_index.x;",
                "    int character = int(255.0*last_index.x);",
                "    int cy = character/32;",
                "    int cx = character-32*cy;",
                "    int px = 8*cx+fx;",
                "    int py = 12*cy+fy;",
                "    float z = texture2D(table, vec2(float(px)/256.0, float(py)/96.0)).x;",
--                 "    z = float(py)/96.0;",
--                 "    z = float(character)/255.0;",
--                 "    z = texture2D(table, texcoord).x;",
                "    gl_FragColor = vec4(z, z, z, 1.0);",
                "}"
           ]

-- | Pair of triangles filling window.
vertices :: V.Vector Float
vertices = V.fromList [ -1.0, -1.0
                      ,  1.0, -1.0 
                      ,  1.0,  1.0 

                      , -1.0, -1.0 
                      ,  1.0,  1.0 
                      , -1.0,  1.0
                      ]

makeMainWindow :: Int -> Int -> IORef (BankersDequeue UIKey) -> IO Window
makeMainWindow screenScaleX' screenScaleY' queue = do
    
    windowHint (WindowHint'OpenGLProfile OpenGLProfile'Any)
    windowHint (WindowHint'DoubleBuffer True)
    windowHint (WindowHint'ContextVersionMajor 2)
    windowHint (WindowHint'ContextVersionMinor 1)

    mWindow <- createWindow (fromIntegral $ screenScaleX'*screenWidth)
                            (fromIntegral $ screenScaleY'*screenHeight)
                            "Stellarator"
                            Nothing
                            Nothing
    case mWindow of
        Nothing -> die "Couldn't create window"
        Just window -> do

            let keyCallback window key someInt state mods = do
                        print (window, key, someInt, state, mods)
                        modifyIORef queue (flip pushBack (UIKey key someInt state mods))
            setKeyCallback window (Just keyCallback)

            makeContextCurrent (Just window)
            print "Created window"

            return window
