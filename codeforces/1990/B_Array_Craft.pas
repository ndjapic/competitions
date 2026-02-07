program B_Array_Craft;
const
    nn = 100 * 1000;
var
    ntc, tci: int16;
    n, i, x, y: int32;
    a: array [1 .. nn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, x, y);

        a[x-1] := 1;
        a[x] := 1;
        for i := x+1 to n do a[i] := -a[i-1];

        a[y] := 1;
        a[y+1] := 1;
        for i := y-1 downto 1 do a[i] := -a[i+1];

        for i := y+2 to x-2 do a[i] := -a[i-1];

        for i := 1 to n-1 do write(a[i], ' ');
        writeln(a[n]);

    end;
end.
