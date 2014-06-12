unit Carpet_Images;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Carpets;

type

  { TCarpetImage }

  TCarpetImage = class(TCustomCarpet)
  private
    FAutoSize: Boolean;
    FPicture: TPicture;
    FStretch: Boolean;
    procedure SetAutoSize(AValue: Boolean);
    procedure SetPicture(AValue: TPicture);
    procedure SetStretch(AValue: Boolean);
  protected
    procedure Paint; override;
    {procedure DefineProperties(Filer: TFiler); override;
    procedure WriteBitmap(AStream: TStream); virtual;
    procedure ReadBitmap(AStream: TStream); virtual;}
    procedure PictureChanged(Sender : TObject); virtual;
    procedure CalcAutoSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color default $1FFFFFFF;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property Picture: TPicture read FPicture write SetPicture;

  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('Standard',[TCarpetImage]);


//  RegisterPropertyEditor(TypeInfo(Cardinal), TCustomCarpet, 'Color', TCarpetColorPropertyEditor);
end;

{ TCarpetImage }

procedure TCarpetImage.SetPicture(AValue: TPicture);
begin
  if FPicture=AValue then Exit;
  //FPicture:=AValue;
  FPicture.Assign(AValue);
end;

procedure TCarpetImage.SetStretch(AValue: Boolean);
begin
  if FStretch=AValue then Exit;
  FStretch:=AValue;
  invalidate;
end;

procedure TCarpetImage.SetAutoSize(AValue: Boolean);
begin
  if FAutoSize=AValue then Exit;
  FAutoSize:=AValue;
  CalcAutoSize;
end;

procedure TCarpetImage.Paint;
var R : TRect;
begin
  inherited Paint;
  if Picture.Graphic = nil
  then Exit;

  if Stretch then
    R := Rect(BorderLeft, BorderTop, Width-BorderRight, Height-BorderBottom)
  else
    R := Rect(0, 0, Picture.Graphic.Width, Picture.Graphic.Height);
  //C.AntialiasingMode := FAntialiasingMode;
  Canvas.StretchDraw(R, Picture.Graphic);

end;

procedure TCarpetImage.PictureChanged(Sender: TObject);
begin
  CalcAutoSize;
end;

procedure TCarpetImage.CalcAutoSize;
begin
  if FAutoSize and (Picture.Graphic <> nil) then
  with Picture.Graphic do
  begin
    if (Width <> self.Width) and (Height <> self.Height) then
       SetBounds(left, top, Width, Height);
  end;
  invalidate;
end;

{procedure TCarpetImage.DefineProperties(Filer: TFiler);
  function DoWrite: Boolean;
  begin
    if (Filer.Ancestor <> nil) and (Filer.Ancestor is TCarpetImage) then
      Result :=  not Equals(Filer.Ancestor)
    else
      Result := Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Bitmap', @ReadBitmap, @WriteBitmap, DoWrite);
end;}

constructor TCarpetImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clNone;
  FAcceptChildrenAtDesignTime:=false;
  FPicture := TPicture.Create;
  FPicture.OnChange := @PictureChanged;

end;

destructor TCarpetImage.Destroy;
begin
  FPicture.OnChange := nil;
  FPicture.Graphic := nil;
  FPicture.Free;
  inherited Destroy;
end;

end.

