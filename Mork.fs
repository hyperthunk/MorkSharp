namespace rec OWLSharp.Extensions.Mork

type Orientation = Up | Down
type PeelState = Peeled | Unpeeled

// This exception depends on the type below.
exception DontSqueezeTheBananaException of Banana
type Banana = {
    PeelState: PeelState
    Orientation: Orientation
}
