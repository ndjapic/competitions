program CodeforcesSortExample;
{$MODE DELPHI}
uses
  SysUtils, 
  Generics.Collections; // Библиотека која садржи TArray.Sort
var
  MojNiz: array of Integer;
  i, n: Integer;

begin
  Readln(n);
  SetLength(MojNiz, n);

  for i := 0 to n - 1 do
    Read(MojNiz[i]);
  ReadLn;
  TArray.Sort<Integer>(MojNiz);

  for i := 0 to n - 1 do
    Write(MojNiz[i], ' ');
  WriteLn;
end.
