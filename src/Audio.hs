module Audio where

{-
sinSamples :: [Int16]
sinSamples =
  map (\n ->
         let t = fromIntegral n / 48000 :: Double
             freq = 440 * 4
         in round (fromIntegral (maxBound `div` 2 :: Int16) * sin (t * freq)))
      [0 :: Integer ..]

audioCB :: IORef [Int16] -> AudioFormat sampleType -> IOVector sampleType -> IO ()
audioCB samples format buffer =
  case format of
    Signed16BitLEAudio ->
      do samples' <- readIORef samples
         let n = V.length buffer
         sequence_ (Prelude.zipWith (write buffer)
                            [0 ..]
                            (Prelude.take n samples'))
         writeIORef samples
                    (Prelude.drop n samples')
    _ -> error "Unsupported audio format"
-}

{-
    samples <- newIORef sinSamples
    (device, _) <- SDL.openAudioDevice OpenDeviceSpec {
        SDL.openDeviceFreq = Mandate 48000,
        SDL.openDeviceFormat = Mandate Signed16BitNativeAudio,
        SDL.openDeviceChannels = Mandate Mono,
        SDL.openDeviceSamples = 4096 * 2,
        SDL.openDeviceCallback = audioCB samples,
        SDL.openDeviceUsage = ForPlayback,
        SDL.openDeviceName = Nothing
    }
    setAudioDevicePlaybackState device Play
    threadDelay 1000000
-}
