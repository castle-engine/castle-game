About index.xml files:

- Various index.xml files within subdirectories describe particular
  resources, like a creature, an item (item is something that can be picked
  and carried by the player) or a level.

  Each index.xml file may contain relative filenames for
  3D models and images related to this resource.
  The idea is that the XML file is kept together with the data of particular
  creature, item etc.
  So you can trivially add/remove available resources
  by simply adding/removing the appropriate subdirectory to the game data.
  No recompilation, and no modification of any central file,
  are necessary to add e.g. a new creature and a new level using that creature.

- Normally, the index.xml files are scanned and read only once when
  the game starts. For easy editing of game data (to not be forced
  to restart the game after every little tweak in configuration),
  you can also use debug menu (under ` key by default)
  to reload XML configuration of various things during the game.
  (Most, but not absolutely all, settings can be changed even while
  the game is running; exceptions are things that are "heavy" ---
  e.g. changing animation filename may require restarting the level.)

- Some attributes specify a sound name.
  For available sound names see names in sounds/index.xml.
  Empty (or not specified) sound name always means "no sound for this event".

- Paths in the XML files should be treated like URLs.
  That is, use slashes as directory separators, and watch out for case.
  The engine will make sure they are handled OK on all platforms.

------------------------------------------------------------------------------
Specific documentation for creature or item kind description,
that is creatures/*/index.xml and items/*/index.xml:

- id: the object id to indicate initial position of this creature in
  the level 3D file. IOW, this determines Blender object name
  to choose this creature type. It must be unique among all resources
  (creature and items kinds).

- type: determines the exact class (ObjectPascal implementation)
  used to instantiate this creature kind.
  It doesn't have to be unique. E.g. creature type "Missile"
  or generic item type "Item" are used by many resources.

  The type determines the behavior that is coded in ObjectPascal
  --- like creature artificial intelligence and whether item can be equipped.

- The type also determines other available attributes of this kind.
  For example, only creature type "WalkAttack" (or it's descendants,
  like "Alien") have the "attack_animation" attribute.

  For the documentation and default values of properties that you can
  set on a creature or item, see T3DResource descendants in the engine:
  TCreatureKind (and descendants) for creatures,
  TItemKind (and descendants) for items.
  They have lot's of properties, and almost all their properties
  can be set by appopriate XML attribute.

