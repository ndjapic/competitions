# Problem: C_Sowing_Stones.pas

```pascal
program C_Sowing_Stones;
{$mode objfpc}{$h+}{$j-}{$inline on}
uses
    math,
    Generics.Defaults, Generics.Collections;
const
    mm = 200 * 1000;
type
    TCell = record
        x, a: int32;
    end;
    TCellList = specialize TList<TCell>;

var
    m, i: int32;
    n, a, d: int64;
    moves: qword;
    loop: boolean;
    c, c1: TCell;
    cells: TCellList;

function CellCompare(constref Left, Right: TCell): int32;
begin
    Result := Left.x - Right.x;
end;

begin
    readln(n, m);
    cells := TCellList.Create();

    for i := 0 to m-1 do begin
        read(c.x);
        cells.Add(c);
    end;
    readln;

    for i := 0 to m-1 do begin
        c := cells[i];
        read(c.a);
        cells[i] := c;
    end;
    readln;

    c.x := n+1;
    c.a := 0;
    cells.Add(c);
    cells.Sort(specialize TComparer<TCell>.Construct(@CellCompare));

    a := 0;
    i := m;
    loop := true;

    while (i > 0) and loop do begin
        dec(i);
        c := cells[i];
        inc(a, c.a);
        loop := a <= n+1 - c.x;
    end;

    if loop then begin

        a := 0;
        moves := 0;
        c1 := cells[0];
        for i := 1 to m do begin

            c := c1;
            c1 := cells[i];
            inc(a, c.a);
            d := min(a, c1.x - c.x);
            dec(a, d);
            inc(moves, (d-1) * d div 2 + a * (c1.x - c.x));

        end;
        writeln(moves);

    end else
        writeln(-1);

end.

```
