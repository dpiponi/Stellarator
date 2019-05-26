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
import SDL (($=))

import System.Exit (exitFailure)
import System.IO
import TIAColors
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import qualified Graphics.Rendering.OpenGL as GL
import qualified SDL
import SDL.Vect

-- | Inform OpenGL about new pixel data.
updateTexture :: GL.TextureObject -> Ptr Word8 -> IO ()
updateTexture texName textureData = do
    GL.textureBinding GL.Texture2D $= Just texName
    GL.texSubImage2D
        GL.Texture2D
        0
        (GL.TexturePosition2D 0 0)
        (GL.TextureSize2D (fromIntegral screenWidth) (fromIntegral screenHeight))
        (GL.PixelData GL.Red GL.UnsignedByte textureData)

-- | Create OpenGL texture map to represent TV screen.
createImageTexture :: GL.TextureObject -> IO (Ptr Word8)
createImageTexture texName = do
    GL.textureBinding GL.Texture2D $= Just texName

    textureData <- mallocBytes (fromIntegral $ screenWidth*screenHeight) :: IO (Ptr Word8)

    forM_ [0..screenHeight-1] $ \i ->
        forM_ [0..screenWidth-1] $ \j -> do
            pokeElemOff textureData (fromIntegral $ screenWidth*i+j) 0

    GL.textureBinding GL.Texture2D $= Just texName
    GL.texImage2D
        GL.Texture2D
        GL.NoProxy
        0
        GL.R8
        (GL.TextureSize2D (fromIntegral screenWidth) (fromIntegral screenHeight))
        0
        (GL.PixelData GL.Red GL.UnsignedByte textureData)

    GL.textureFilter   GL.Texture2D   $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)

    return textureData

-- | Create 1D texture map to represent colour lookup table as described at
--   https://en.wikipedia.org/wiki/Television_Interface_Adaptor#TIA_Color_Capabilities
createLUTTexture :: GL.TextureObject -> IO ()
createLUTTexture texName = do
    textureData2 <- mallocBytes (4*256) :: IO (Ptr Word32)
    forM_ [0..255] $ \i ->
        pokeElemOff textureData2 (fromIntegral $ i) (fromIntegral $ lut!(i `shift` (-1)))

    GL.textureBinding GL.Texture1D $= Just texName

    GL.texImage1D
        GL.Texture1D
        GL.NoProxy
        0
        GL.RGB8
        (GL.TextureSize1D 256)
        0
        (GL.PixelData GL.BGRA GL.UnsignedByte textureData2)

    GL.textureFilter   GL.Texture1D   $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureWrapMode GL.Texture1D GL.S $= (GL.Repeated, GL.ClampToEdge)
    --GL.textureWrapMode GL.Texture1D GL.T $= (GL.Repeated, GL.ClampToEdge)

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
connectProgramToTextures program alpha current_frame_tex last_frame_tex lut_tex = do
    GL.currentProgram $= Just program
    current_screen_tex_loc <- GL.uniformLocation program "current_frame"
    last_screen_tex_loc <- GL.uniformLocation program "last_frame"
    lutTexLoc <- GL.uniformLocation program "table"
    alpha_loc <- GL.uniformLocation program "alpha"

    GL.uniform alpha_loc $= alpha

    GL.activeTexture $= GL.TextureUnit 0
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just current_frame_tex

    GL.activeTexture $= GL.TextureUnit 1
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just last_frame_tex

    GL.activeTexture $= GL.TextureUnit 2
    GL.texture GL.Texture1D $= GL.Enabled
    GL.textureBinding GL.Texture1D $= Just lut_tex

    GL.uniform current_screen_tex_loc $= GL.Index1 (0::GL.GLint)
    GL.uniform last_screen_tex_loc $= GL.Index1 (1::GL.GLint)
    GL.uniform lutTexLoc $= GL.Index1 (2::GL.GLint)

    GL.validateProgram program
    status <- GL.get $ GL.validateStatus program
    unless status $ do
        hPutStrLn stderr "GL.linkProgram error"
        plog <- GL.get $ GL.programInfoLog program
        putStrLn plog
        exitFailure
    GL.currentProgram $= Just program

-- | Create all OpenGL objects required including shaders and textures.
initResources :: Float -> IO (GL.Program, GL.AttribLocation, GL.TextureObject, GL.TextureObject, Ptr Word8, Ptr Word8)
initResources alpha = do
    [current_frame_tex, last_frame_tex, lut_tex] <- GL.genObjectNames 3 :: IO [GL.TextureObject]

    textureData <- createImageTexture current_frame_tex
    lastTextureData <- createImageTexture last_frame_tex
    createLUTTexture lut_tex

    program <- createShaderProgram
    connectProgramToTextures program alpha current_frame_tex last_frame_tex lut_tex

    return (program, GL.AttribLocation 0, current_frame_tex, last_frame_tex, textureData, lastTextureData)

-- | Render VCS screen as pair of triangles.
draw :: Int -> Int -> GL.Program -> GL.AttribLocation -> IO ()
draw windowWidth windowHeight program attrib = do
    GL.clearColor $= GL.Color4 0 0 0 0
    GL.clear [GL.ColorBuffer]
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral windowWidth) (fromIntegral windowHeight))

    GL.currentProgram $= Just program
    GL.vertexAttribArray attrib $= GL.Enabled
    V.unsafeWith vertices $ \ptr ->
        GL.vertexAttribPointer attrib $=
          (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 ptr)
    GL.drawArrays GL.Triangles 0 6
    GL.vertexAttribArray attrib $= GL.Disabled
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
                "uniform sampler1D table;",
                "uniform float alpha;",
                "varying vec2 texcoord;",
                "",
                "void main()",
                "{",
                "",
                "    vec4 current_index = texture2D(current_frame, texcoord);",
                "    vec4 last_index = texture2D(last_frame, texcoord);",
                "    gl_FragColor = alpha*texture1D(table, current_index.x)+(1.0-alpha)*texture1D(table, last_index.x);",
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

makeMainWindow :: Int -> Int -> IO SDL.Window
makeMainWindow screenScaleX' screenScaleY' = do
    window <- SDL.createWindow "Stellarator"
                SDL.defaultWindow {
--                     SDL.windowMode = SDL.FullscreenDesktop,
                    SDL.windowInitialSize = V2 (fromIntegral $ screenScaleX'*screenWidth)
                    (fromIntegral $ screenScaleY'*screenHeight),
                    SDL.windowOpenGL = Just $ SDL.OpenGLConfig {
                        SDL.glColorPrecision = V4 8 8 8 0,
                        SDL.glDepthPrecision = 24,
                        SDL.glStencilPrecision = 8,
                        SDL.glMultisampleSamples = 1,
                        SDL.glProfile = SDL.Compatibility SDL.Normal 2 1
                        }}

    SDL.showWindow window
    _ <- SDL.glCreateContext window
    SDL.swapInterval $= SDL.SynchronizedUpdates
--     SDL.swapInterval $= SDL.ImmediateUpdates

    return window
