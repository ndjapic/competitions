# Problem: Double_or_One_Thing.pas

```pascal
program Double_or_One_Thing;
var
    notc, n, i: int8;
    s: array [1 .. 100] of char;
    h: array [1 .. 100] of boolean;

begin
    readln(notc);
    repeat

        n := 0;
        repeat
            inc(n);
            read(s[n]);
        until eoln;
        readln;

        h[n] := false;
        for i := n-1 downto 1 do
            if s[i+1] < s[i] then
                h[i] := false
            else if s[i+1] > s[i] then
                h[i] := true
            else
                h[i] := h[i+1];

        for i := 1 to n do begin
            write(s[i]);
            if h[i] then write(s[i]);
        end;
        writeln;

        dec(notc);
    until notc = 0;
end.


```
