{ Keys of the game.
  TODO: make keys configurable in game. }
unit CastleKeys;

interface

uses Keys, MatrixNavigation;

const
  { Basic keys. }

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

  { Items keys. }
  CastleKey_InventoryShow = K_I;
  CastleKey_InventoryPrevious = K_LeftBracket;
  CastleKey_InventoryNext = K_RightBracket;
  CastleKey_DropItem = K_D;
  CastleKey_UseItem = K_Enter;

  { Other keys. }

  CastleKey_ShowHelp = K_F1;
  CastleKey_SaveScreen = K_F5;
  CastleKey_ViewMessages = K_M;
  CastleKey_CancelFlying = K_C;

implementation

end.
