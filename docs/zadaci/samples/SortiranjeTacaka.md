# Задатак: SortiranjeTacaka.pas

```pascal
program StabilnoSortiranje;

{$MODE DELPHI}
{$H+}

uses
  SysUtils, Generics.Collections;

type
  TTacka = record
    x, y: Integer;
  end;

function UporediTacke(constref Left, Right: TTacka): Integer;
begin
  if Left.y <> Right.y then
    Result := Right.y - Left.y // Већи Y иде први (одозго)
  else
    Result := Left.x - Right.x; // Мањи X иде први (слева)
end;

{ 
  Функција враћа индекс на који треба уметнути елемент.
  Сложеност: O(log N)
}
function bisect(Lista: TList<TTacka>; Trazena: TTacka): Integer;
var
  Low, High, Mid: Integer;
begin
  Low := 0;
  High := Lista.Count - 1;
  Result := Lista.Count; // Ако је већи од свих, иде на крај

  while Low <= High do
  begin
    Mid := Low + (High - Low) div 2;
    // Ако је Тражена тачка "мања" (иде после у тексту) од оне на Mid
    if UporediTacke(Lista[Mid], Trazena) < 0 then
      Low := Mid + 1
    else
    begin
      Result := Mid;
      High := Mid - 1;
    end;
  end;
end;

function exists(Lista: TList<TTacka>; Trazena: TTacka): Boolean;
var
  Idx: Integer;
begin
  Idx := bisect(Lista, Trazena);
  // Тачка постоји ако индекс није "на крају" листе
  // и ако су координате тачке на том индексу идентичне траженој
  Result := (Idx < Lista.Count) and 
            (Lista[Idx].x = Trazena.x) and 
            (Lista[Idx].y = Trazena.y);
end;

var
  Lista: TList<TTacka>;
  T: TTacka;
  Idx: Integer;

begin
  Lista := TList<TTacka>.Create;
  try
    // 1. Уметнимо прве две тачке ручно (сортирано)
    T.x := 10; T.y := 100; Lista.Add(T);
    T.x := 20; T.y := 50;  Lista.Add(T);

    // 2. Тачка која треба да иде ИЗМЕЂУ њих (y=80)
    T.x := 15; T.y := 80;

    // 3. Проналажење индекса помоћу наше функције
    Idx := bisect(Lista, T);
    
    WriteLn(Format('Tacka (y=%d) se umece на индекс: %d', [T.y, Idx]));
    Lista.Insert(Idx, T);

    // 4. Провера редоследа
    WriteLn('Trenutni redosled u listi:');
    for Idx := 0 to Lista.Count - 1 do
      WriteLn(Format('%d. Tacka(x: %d, y: %d)', [Idx, Lista[Idx].x, Lista[Idx].y]));

  finally
    Lista.Free;
  end;
end.

```
