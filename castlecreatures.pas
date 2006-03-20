unit CastleCreatures;

interface

{$define read_interface}

type
  TCreatureKind = class
  public
    { This is an animation of standing still.
      Beginning and end of it must glue together. }
    property StandAnimation: TVRMLGLAnimation read FStandAnimation;

    { This is an animation when he changes from standing still to walking.
      It's frame 0 must glue with frame 0 of StandAnimation,
      it's last frame must glue with frame 0 of WalkAnimation. }
    property StandToWalkAnimation: TVRMLGLAnimation read FStandToWalkAnimation;

    { This is an animation of walking.
      Beginning and end of it must glue together. }
    property WalkAnimation: TVRMLGLAnimation read FWalkAnimation;

    { This is an animation of attacking.
      If AttacksWhenWalking then beginning and end of it must glue
      with frame 0 of WalkAnimation.
      Else beginning and end of it must glue
      with frame 0 of StandAnimation. }
    property AttackAnimation: TVRMLGLAnimation read FAttackAnimation;

    { See @link(AttackAnimation). }
    property AttacksWhenWalking: boolean read FAttacksWhenWalking;
  end;

  TCreature = class
  private
    FPosition: TVector3Single;
    FDirection: TVector3Single;
  public
    property Kind: TCreatureKind read FKind;

    function BoundingBox: TBox3d; virtual; abstract;

    procedure Render; virtual; abstract;

    { This is the position of the (0, 0, 0) point of creature model
      (or rather, currently used model! Creatures are animated after all). }
    property Position: TVector3Single read FPosition;

    { Direction the creature is facing. }
    property Direction: TVector3Single read FDirection;
  end;

implementation

end.