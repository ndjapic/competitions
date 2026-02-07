program C_Spiral_Rotation;
{$mode delphi}
uses
    math;
const
    nn = 3000;
var
    n, i: int32;
    j: int8;
    a, b: array [1 .. nn] of string;

procedure rotate(i: int16);
var
    x, y: int16;
begin
    y := i;
    for x := i+1 to n+1-i do b[y][n+1-x] := a[x][y];
    x := n+1-i;
    for y := i+1 to n+1-i do b[y][n+1-x] := a[x][y];
    y := n+1-i;
    for x := i to n-i do b[y][n+1-x] := a[x][y];
    x := i;
    for y := i to n-i do b[y][n+1-x] := a[x][y];

    y := i;
    for x := i+1 to n+1-i do a[x][y] := b[x][y];
    x := n+1-i;
    for y := i+1 to n+1-i do a[x][y] := b[x][y];
    y := n+1-i;
    for x := i to n-i do a[x][y] := b[x][y];
    x := i;
    for y := i to n-i do a[x][y] := b[x][y];
end;

begin
    readln(n);
    for i := 1 to n do begin
        readln(a[i]);
        setlength(b[i], n);
    end;

    for i := 1 to n div 2 do
        for j := 1 to i mod 4 do rotate(i);

    for i := 1 to n do writeln(a[i]);
end.
