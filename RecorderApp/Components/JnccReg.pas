unit JnccReg;

interface

uses
  Classes, DsgnIntf, SpatialRef, Measurements, Sources, VagueDateEdit,
  JnccDatasets, JnccGrid, Finder;

procedure Register;

//==============================================================================
implementation

//==============================================================================
procedure Register;
begin
  RegisterComponents('JNCC', [TSpatialRef,
                              TMeasurements,
                              TSources,
                              TVagueDateEdit,
                              TJnccTable,
                              TJnccQuery,
                              TJNCCNamesQuery,
                              TDBJnccGrid,
                              TFinder]);
end;  { Register }

//==============================================================================
end.

