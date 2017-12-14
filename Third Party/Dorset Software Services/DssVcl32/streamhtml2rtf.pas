{ ------------------------------------------------------------------------------
// Unit:          streamhtml2rtf
//
//                Copyright © Dorset Software Services Ltd, 1999
//
// Description:   HTML <-> RTF conversion via streams
//
// Author:        AJWK
//
// Created:       1999-09-16
//
// Changes:       RTF->HTML - Ian Addis - 18/07/2000
//
// To Do:
// ----------------------------------------------------------------------------}
unit streamhtml2rtf;

interface

uses
  Classes;


procedure StreamHTMLToRTF(
    htmlStream: TStream; rtfStream: TStream; bytes: Longint
);

procedure StreamRTFToHTML(rtfStream: TStream; htmlStream: TStream);



implementation

uses
  html2rtf;

const
  BUFFER_SIZE = 2048;


{ ------------------------------------------------------------------------------
// StreamHTMLToRTF
//
// Simple conversion from HTML to RTF.
//
// -- reads from the current position of htmlStream
//
// -- reads at most the specified number of bytes of input.
// ----------------------------------------------------------------------------}
procedure StreamHTMLToRTF(
    htmlStream: TStream; rtfStream: TStream; bytes: Longint
);
var
  convertor:  THTMLRTFConversion;
  buffer:     array[0..BUFFER_SIZE - 1] of Char;
  bytesread:  Longint;

begin
  convertor := THTMLRTFConversion.Create;
  try
    {set limits on the input stream}
    convertor.ConvertStartOffset := htmlStream.Position;
    convertor.ConvertEndOffset := htmlStream.Position + bytes;

    if convertor.ConvertEndOffset > htmlStream.Size then begin
      convertor.ConvertEndOffset := 0;
    end;

    {perform the conversion}
    bytesread := convertor.ConvertReadStream(htmlStream, buffer, BUFFER_SIZE);
    while bytesread > 0 do begin
      rtfStream.Write(buffer, bytesread);
      bytesread := convertor.ConvertReadStream(htmlStream, buffer, BUFFER_SIZE);
    end;  {while bytesread > 0}

  finally
    convertor.Free;
  end;  {try .. finally}
end;  {StreamHTMLToRTF}

{ ------------------------------------------------------------------------------
// StreamRTFToHTML
//
// Ian Addis
//
// Simple conversion from RTF to HTML.
//
// -- reads from the current position of rtfStream
// ----------------------------------------------------------------------------}
procedure StreamRTFToHTML(rtfStream: TStream; htmlStream: TStream);
var
  convertor: THTMLRTFConversion;
  pBuffer: PChar;
{  iBytesWritten: Integer;}
begin

  //Create converter
  convertor:= THTMLRTFConversion.Create;

  //Allocate space for buffer (plus trailing null)
  GetMem(pBuffer, rtfStream.Size + 1);

  //Create rtf stream in buffer
  rtfStream.ReadBuffer(pBuffer^, rtfStream.Size);

  //Perform the conversion
  {iBytesWritten:= }convertor.ConvertWriteStream(htmlStream, pBuffer, rtfStream.Size);

  //Free space in buffer
  Dispose(pBuffer);

  //Free converter
  convertor.Free;
end;  {StreamRTFToHTML}

end.
