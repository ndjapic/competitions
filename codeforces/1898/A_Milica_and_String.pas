program A_Milica_and_String;
uses
    math;
const
    maxn = 500;
    maxm = 250 * 501;
var
    ntc, tci: int16;
    n, k, i: int8;
    s: array [1 .. maxn] of char;
    b: array [0 .. maxn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n, k);

        b[0] := 0;
        for i := 1 to n do begin
            read(s[i]);
            b[i] := b[i-1];
            if s[i] = 'B' then inc(b[i]);
        end;
        readln;

        if b[n] > k then begin

            i := 0;
            while b[n] - b[i] > k do inc(i);
            writeln(1);
            writeln(i, ' A');

        end else if b[n] < k then begin

            i := 0;
            while b[n] - b[i] + i < k do inc(i);
            writeln(1);
            writeln(i, ' B');

        end else
            writeln(0);
    end;
end.
