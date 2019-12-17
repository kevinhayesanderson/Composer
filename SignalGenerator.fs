module SignalGenerator

let generateSamples milliseconds frequency =
    let volume = 0.8
    let sixteenBitSampleLimit = 32767.
    let sampleRate = 44100.
    let toAmplitude x =
        x 
        |> (*) (2. * System.Math.PI * frequency / sampleRate)
        |> sin
        |> (*) sixteenBitSampleLimit 
        |> (*) volume
        |> int16

    let requiredSamples = seq { 1.0..(milliseconds / 1000. * sampleRate) }
    Seq.map toAmplitude requiredSamples
    
