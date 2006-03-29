{ Keys of the game.
  TODO: make keys configurable in game. }
unit CastleKeys;

interface

uses Keys, MatrixNavigation;

const
  CastleKey_UpMove = WalkerDefaultKey_Jump;
  CastleKey_DownMove = WalkerDefaultKey_Crouch;
  CastleKey_Forward = WalkerDefaultKey_Forward;
  CastleKey_Backward = WalkerDefaultKey_Backward;
  CastleKey_LeftRot = WalkerDefaultKey_LeftRot;
  CastleKey_RightRot = WalkerDefaultKey_RightRot;
  CastleKey_LeftStrafe = WalkerDefaultKey_LeftStrafe;
  CastleKey_RightStrafe = WalkerDefaultKey_RightStrafe;
  CastleKey_UpRotate = WalkerDefaultKey_UpRotate;
  CastleKey_DownRotate = WalkerDefaultKey_DownRotate;
  CastleKey_HomeUp = WalkerDefaultKey_HomeUp;
  CastleKey_Attack = K_Ctrl;

implementation

end.
