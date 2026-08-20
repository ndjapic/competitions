program A_Common_Multiple;
const
    nn = 100;
var
    ntc, tci: int16;
    n, i, x, ans: int8;
    seen: array [1 .. nn] of boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for x := 1 to n do seen[x] := false;

        ans := 0;
        for i := 1 to n do begin
            read(x);
            if not seen[x] then begin
                inc(ans);
                seen[x] := true;
            end;
        end;
        readln;

        writeln(ans);

    end;
end.
