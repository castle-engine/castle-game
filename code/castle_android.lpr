{$mode objfpc}{$H+}
library castle_android;
uses CastleAndroidNativeAppGlue, Game, CastleMessaging;
exports
  Java_net_sourceforge_castleengine_MainActivity_jniMessage,
  ANativeActivity_onCreate;
end.
