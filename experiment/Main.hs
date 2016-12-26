--port of https://github.com/bergey/haskell-OpenGL-examples

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import TIAColors
import Data.Bits
import Data.Array.Unboxed
import Control.Monad
import Foreign.C.Types
import SDL.Vect
import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V
import           System.Exit (exitFailure)
import           System.IO

import SDL (($=))
import qualified SDL
import qualified Graphics.Rendering.OpenGL as GL

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.Word
import Foreign.Storable

windowWidth, windowHeight :: CInt
(windowWidth, windowHeight) = (2000, 1600)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (64, 48)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  -- SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  -- do renderQuality <- SDL.get SDL.HintRenderScaleQuality
  --    when (renderQuality /= SDL.ScaleLinear) $
  --      putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow {SDL.windowInitialSize = V2 windowWidth windowHeight,
                         SDL.windowOpenGL = Just SDL.defaultOpenGL}
  SDL.showWindow window

  _ <- SDL.glCreateContext window
  SDL.swapInterval $= SDL.SynchronizedUpdates
  (prog, attrib, tex) <- initResources

  let loop i = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        GL.clear [GL.ColorBuffer]
        draw prog attrib
        SDL.glSwapWindow window
        createImageTexture tex i

        unless quit $ loop (i+1)

  loop 0

  SDL.destroyWindow window
  SDL.quit

createImageTexture :: GL.TextureObject -> Int -> IO ()
createImageTexture texName offset = do
    GL.textureBinding GL.Texture2D $= Just texName

    textureData <- mallocBytes (fromIntegral $ screenWidth*screenHeight) :: IO (Ptr Word8)
    forM_ [0..screenHeight-1] $ \i ->
        forM_ [0..screenWidth-1] $ \j -> do
            pokeElemOff textureData (fromIntegral $ screenWidth*i+j) (fromIntegral $ (j+fromIntegral offset))

    putStrLn "Image texture created"
    GL.texImage2D
        GL.Texture2D
        GL.NoProxy
        0
        GL.R8
        (GL.TextureSize2D (fromIntegral screenWidth) (fromIntegral screenHeight))
        0
        (GL.PixelData GL.Red GL.UnsignedByte textureData)

    putStrLn "Texture loaded."

    GL.textureFilter   GL.Texture2D   $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)

createLUTTexture :: GL.TextureObject -> IO ()
createLUTTexture texName = do
    textureData2 <- mallocBytes (fromIntegral $ 4*256) :: IO (Ptr Word32)
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
    GL.textureWrapMode GL.Texture1D GL.T $= (GL.Repeated, GL.ClampToEdge)

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

    print $ linkOK

    unless linkOK $ do
        hPutStrLn stderr "GL.linkProgram error"
        plog <- GL.get $ GL.programInfoLog program
        putStrLn plog
        exitFailure

    return program

connectProgramToTextures :: GL.Program -> GL.TextureObject -> GL.TextureObject -> IO ()
connectProgramToTextures program tex tex2 = do
    GL.currentProgram $= Just program
    texLoc <- GL.uniformLocation program "texture"
    print texLoc
    texLoc2 <- GL.uniformLocation program "table"
    print texLoc2

    GL.activeTexture $= GL.TextureUnit 0
    GL.texture GL.Texture2D $= GL.Enabled
    GL.textureBinding GL.Texture2D $= Just tex

    GL.activeTexture $= GL.TextureUnit 1
    GL.textureBinding GL.Texture1D $= Just tex2
    GL.texture GL.Texture1D $= GL.Enabled

    GL.uniform texLoc $= GL.Index1 (0::GL.GLint)
    GL.uniform texLoc2 $= GL.Index1 (1::GL.GLint)

    GL.validateProgram program
    status <- GL.get $ GL.validateStatus program
    print $ status
    unless status $ do
        hPutStrLn stderr "GL.linkProgram error"
        plog <- GL.get $ GL.programInfoLog program
        putStrLn plog
        exitFailure
    GL.currentProgram $= Just program

initResources :: IO (GL.Program, GL.AttribLocation, GL.TextureObject)
initResources = do
    [tex, tex2] <- GL.genObjectNames 2 :: IO [GL.TextureObject]

    createImageTexture tex 0
    createLUTTexture tex2

    program <- createShaderProgram
    connectProgramToTextures program tex tex2

    return (program, GL.AttribLocation 0, tex)

draw :: GL.Program -> GL.AttribLocation -> IO ()
draw program attrib = do
    GL.clearColor $= GL.Color4 0 0 0 0
    GL.clear [GL.ColorBuffer]
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral windowWidth) (fromIntegral windowHeight))

    GL.currentProgram $= Just program
    GL.vertexAttribArray attrib $= GL.Enabled
    V.unsafeWith vertices $ \ptr ->
        GL.vertexAttribPointer attrib $=
          (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 ptr)
    GL.drawArrays GL.Triangles 0 6 -- 3 is the number of vertices
    GL.vertexAttribArray attrib $= GL.Disabled

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
                "    texcoord = position * vec2(0.5) + vec2(0.5);",
                "}"
           ]

fsSource = BS.intercalate "\n"
           [
                "#version 110",
                "",
                "uniform sampler2D texture;",
                "uniform sampler1D table;",
                "varying vec2 texcoord;",
                "",
                "void main()",
                "{",
                "",
                "    vec4 index = texture2D(texture, texcoord);",
                "    gl_FragColor = texture1D(table, index.x);",
                "}"
           ]

vertices :: V.Vector Float
vertices = V.fromList [ -1.0, -1.0
                      ,  1.0, -1.0 
                      ,  1.0,  1.0 
                      , -1.0, -1.0 
                      ,  1.0,  1.0 
                      , -1.0,  1.0
                      ]
