--port of https://github.com/bergey/haskell-OpenGL-examples

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

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

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight,
                         SDL.windowOpenGL = Just SDL.defaultOpenGL}
  SDL.showWindow window

  _ <- SDL.glCreateContext window
  (prog, attrib) <- initResources

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        GL.clear [GL.ColorBuffer]
        draw prog attrib
        SDL.glSwapWindow window

        unless quit loop

  loop

  SDL.destroyWindow window
  SDL.quit

initResources :: IO (GL.Program, GL.AttribLocation)
initResources = do
    [tex] <- GL.genObjectNames 1 :: IO [GL.TextureObject]
    GL.texture GL.Texture2D $= GL.Enabled
    GL.activeTexture $= GL.TextureUnit 0
    GL.textureBinding GL.Texture2D $= Just tex

    textureData <- mallocBytes (fromIntegral $ screenWidth*screenHeight*4) :: IO (Ptr Word8)
    forM_ [0..screenHeight-1] $ \i ->
        forM_ [0..screenWidth-1] $ \j -> do
            pokeElemOff textureData (fromIntegral $ 4*screenWidth*i+4*j+0) (fromIntegral $ (i `div` 2))
            pokeElemOff textureData (fromIntegral $ 4*screenWidth*i+4*j+1) (fromIntegral $ (j `div` 2))
            pokeElemOff textureData (fromIntegral $ 4*screenWidth*i+4*j+2) (fromIntegral $ 128)
            pokeElemOff textureData (fromIntegral $ 4*screenWidth*i+4*j+3) (fromIntegral $ 255)

    putStrLn "Buffering glyph bitmap into texture."
    GL.texImage2D
        GL.Texture2D
        GL.NoProxy
        0
        GL.RGB8
        (GL.TextureSize2D (fromIntegral screenWidth) (fromIntegral screenHeight))
        0
        (GL.PixelData GL.RGBA GL.UnsignedByte textureData)

    putStrLn "Texture loaded."


    GL.textureFilter   GL.Texture2D   $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)

    [tex2] <- GL.genObjectNames 1 :: IO [GL.TextureObject]

    textureData2 <- mallocBytes (fromIntegral $ 256*4) :: IO (Ptr Word8)
    forM_ [0..255] $ \i -> do
            pokeElemOff textureData2 (fromIntegral $ 3*i+0) (fromIntegral $ i)
            pokeElemOff textureData2 (fromIntegral $ 3*i+1) (fromIntegral $ 255-i)
            pokeElemOff textureData2 (fromIntegral $ 3*i+2) (fromIntegral $ 128)
            pokeElemOff textureData2 (fromIntegral $ 3*i+3) (fromIntegral $ 255)

    GL.texture GL.Texture1D $= GL.Enabled
    GL.activeTexture $= GL.TextureUnit 1
    GL.textureBinding GL.Texture1D $= Just tex2

    GL.texImage1D
        GL.Texture1D
        GL.NoProxy
        0
        GL.RGB8
        (GL.TextureSize1D 256)
        0
        (GL.PixelData GL.RGB GL.UnsignedByte textureData2)

    GL.textureFilter   GL.Texture1D   $= ((GL.Nearest, Nothing), GL.Nearest)
    GL.textureWrapMode GL.Texture1D GL.S $= (GL.Repeated, GL.ClampToEdge)
{-
    -}

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
    GL.validateProgram program
    status <- GL.get $ GL.validateStatus program
    unless (linkOK && status) $ do
        hPutStrLn stderr "GL.linkProgram error"
        plog <- GL.get $ GL.programInfoLog program
        putStrLn plog
        exitFailure
    GL.currentProgram $= Just program

    return (program, GL.AttribLocation 0)

draw :: GL.Program -> GL.AttribLocation -> IO ()
draw program attrib = do
    GL.clearColor $= GL.Color4 0 0 0 0
    GL.clear [GL.ColorBuffer]
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral screenWidth) (fromIntegral screenHeight))

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
                "    gl_FragColor = ",
                "        texture2D(texture, texcoord);",
                "}"
           ]
           {-
                "        texture1D(table, texcoord.x);",
           -}

vertices :: V.Vector Float
vertices = V.fromList [ -1.0, -1.0
                      ,  1.0, -1.0 
                      ,  1.0,  1.0 
                      , -1.0, -1.0 
                      ,  1.0,  1.0 
                      , -1.0,  1.0
                      ]
