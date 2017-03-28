unit adsImageMethods;

{ © 2002-2010 Aston Design Studio }

interface

uses
  Classes, Graphics, ExtCtrls;

type
  TCompareUpdate = procedure(Current, Total: Integer) of object;

{** This function loads the two images represented by ImageFile1 and ImageFile2,
 looks at their dimensions and at NoOfPoints different pixels in the images. If
 both the dimensions and the color value at these points are the same in both
 images, this means that the images may be equal and the method returns True,
 otherwise it returns False. }
function ImagesMayBeEqual(ImageFile1, ImageFile2: String;
  NoOfPoints: Integer=100): Boolean;

{** This method looks at all images in Images, and those that may be the same
 are stored in CompareResult as 'Image1.jpg - Image2.jpg. }
procedure CompareImages(Images, CompareResult: TStrings;
  CompareUpdate: TCompareUpdate);

implementation

uses
  Math, SysUtils, adsStringMethods;

function ImagesMayBeEqual(ImageFile1, ImageFile2: String;
  NoOfPoints: Integer=100): Boolean;
var
  i, x, y: Integer;
  Img1, Img2: TImage;
  Bmp1, Bmp2: TBitmap;
begin
  Result := True;
  Img1 := TImage.Create(nil);
  Bmp1 := TBitmap.Create;
  Img2 := TImage.Create(nil);
  Bmp2 := TBitmap.Create;
  Img1.Picture.LoadFromFile(ImageFile1);
  Img2.Picture.LoadFromFile(ImageFile2);
  if (Img1.Picture.Width <> Img2.Picture.Width) or
    (Img1.Picture.Height <> Img2.Picture.Height) then
    Result := False
  else
    try
      Randomize;
      Bmp1.Assign(Img1.Picture.Graphic);
      Bmp2.Assign(Img2.Picture.Graphic);
      for i := 0 to NoOfPoints-1 do
      begin
        x := Random(Img1.Picture.Width);
        y := Random(Img1.Picture.Height);
        if Bmp1.Canvas.Pixels[x, y] <>
          Bmp2.Canvas.Pixels[x, y] then
        begin
          Result := False;
          Break;
        end; {if}
      end; {for}
  finally
    Img1.Free;
    Img2.Free;
    Bmp1.Free;
    Bmp2.Free;
  end; {try/finally}
end; {procedure}

function StringValue(ImageFile1: String): String;
var
  i, j, Value: Integer;
  Img1: TImage;
  Bmp1: TBitmap;
begin
  Value := 0;
  Img1 := TImage.Create(nil);
  Bmp1 := TBitmap.Create;
  Img1.Picture.LoadFromFile(ImageFile1);
  Bmp1.Assign(Img1.Picture.Graphic);
  try
    Result := IntToStr(Img1.Picture.Width);
    Result := Result + IntToStr(Img1.Picture.Height);
    for i := 0 to Min(Img1.Picture.Width-1, 25) do
      for j := 0 to Min(Img1.Picture.Height, 25) do
        Value := Value + Bmp1.Canvas.Pixels[i, j];
    Result := Result + IntToStr(Value);
  finally
    Img1.Free;
    Bmp1.Free;
  end; {try/finally}
end; {procedure}

procedure CompareImages(Images, CompareResult: TStrings;
  CompareUpdate: TCompareUpdate);
var
  i, Total: Integer;
  ImageValues: TStringList;
begin
  ImageValues := TStringList.Create;
  try
    CompareResult.Clear;
    Total := Images.Count;
    for i := 0 to Images.Count-1 do
    begin
      ImageValues.Add(StringValue(Images[i])+';'+Images[i]);
      if Assigned(CompareUpdate) then
        CompareUpdate(i+1, Total);
    end; {for}
    ImageValues.Sort;
    for i := 1 to ImageValues.Count-1 do
      if LeftPart(ImageValues[i-1], ';') = LeftPart(ImageValues[i], ';') then
        CompareResult.Add(RightPart(ImageValues[i-1], ';') + ' - ' +
          RightPart(ImageValues[i], ';'));
  finally
    ImageValues.Free;
  end; {finally}
end; {procedure}

end.
