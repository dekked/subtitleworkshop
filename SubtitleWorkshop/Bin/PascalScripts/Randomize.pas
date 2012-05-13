// -------------------------------------------------------------------------- //
//            Subtitle Workshop - Randomize subtitles pascal script           //
//                        Copyright © 2001-2004 URUSoft                       //
//                           http://www.urusoft.net                           //
//                                                                            //
// This script will randomize the positions of all the subtitles in a file.   //
// The purpose of it is to be able to go through a subtitle file, correcting  //
// grammar/spelling mistakes without knowing really what is happening in the  //
// movie.                                                                     //
//                                                                            //
// Usage: first, use this randomize script via Tools/Pascal scripts menu,     //
//        then correct the subtitles and then sort them again using "Sort"    //
//        feature.                                                            //
//                                                                            //
// -------------------------------------------------------------------------- //

program RandomizeSubs;

// -----------------------------------------------------------------------------

var
  i              : Integer;
  r              : Integer;
  Count          : Integer;
  tmpInitialTime : Integer;
  tmpFinalTime   : Integer;
  tmpText        : String;
  tmpTrans       : String;
begin
  Count := GetSubtitleCount;

  for i := 0 to Count-1 do
  begin
    Randomize;
    r := Random(Count-1);

    // Exchange subtitles
    tmpInitialTime := GetSubtitleInitialTime(i);
    tmpFinalTime   := GetSubtitleFinalTime(i);
    tmpText        := GetSubtitleText(i);
    tmpTrans       := GetSubtitleTrans(i);

    SetSubtitleInitialTime(i, GetSubtitleInitialTime(r));
    SetSubtitleFinalTime(i, GetSubtitleFinalTime(r));
    SetSubtitleText(i, GetSubtitleText(r));
    SetSubtitleTrans(i, GetSubtitleTrans(r));

    SetSubtitleInitialTime(r, tmpInitialTime);
    SetSubtitleFinalTime(r, tmpFinalTime);
    SetSubtitleText(r, tmpText);
    SetSubtitleTrans(r, tmpTrans);
  end;
end.

// -----------------------------------------------------------------------------

end.
