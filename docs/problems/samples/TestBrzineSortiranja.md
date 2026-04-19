# Problem: TestBrzineSortiranja.pas

```pascal
program TestBrzineSortiranja;

{$MODE DELPHI}
{$H+}

uses
  SysUtils, 
  Generics.Collections, 
  Generics.Defaults;

type
  TTacka = record
    x, y: Integer;
  end;

function UporediTacke(constref Left, Right: TTacka): Integer;
begin
  if Left.y <> Right.y then
    Result := Right.y - Left.y 
  else
    Result := Left.x - Right.x;
end;

var
  Lista: TList<TTacka>;
  i: Integer;
  T: TTacka;
  StartVreme, KrajVreme: QWord; // Користимо QWord за милисекунде
  n: Integer = 1000000;

begin
  Randomize;
  Lista := TList<TTacka>.Create;
  try
    // 1. Попуњавање насумичним подацима
    WriteLn('Generisem podatke...');
    for i := 1 to n do
    begin
      T.x := Random(1000);
      T.y := Random(1000);
      Lista.Add(T);
    end;

    WriteLn(Format('Zapocinjem sortiranje %d tacaka...', [n]));

    // 2. Мерење почетног времена
    StartVreme := GetTickCount64;

    // Сортирање
    Lista.Sort(TComparer<TTacka>.Construct(UporediTacke));

    // 3. Мерење крајњег времена
    KrajVreme := GetTickCount64;

    WriteLn('Sortiranje zavrseno.');
    WriteLn(Format('Proteklo vreme: %d ms', [KrajVreme - StartVreme]));

  finally
    Lista.Free;
  end;
  
  // Ово је опционо, за заустављање терминала ако не покрећеш из конзоле
  // Write('Pritisnite Enter za kraj...');
  // Readln;
end.

```
