program C_Black_Intervals;
{$mode delphi}{$inline on}
const
    nn = 500 * 1000 + 1;
var
    n, q, i, x, ans: int32;
    black: array [0 .. nn] of boolean;

begin
    readln(n, q);

    for x := 0 to n+1 do black[x] := false;

    ans := 0;
    for i := 1 to q do begin
        read(x);
        if black[x] then begin

            if black[x-1] and black[x+1] then
                inc(ans)
            else if not black[x-1] and not black[x+1] then
                dec(ans);

        end else begin

            if black[x-1] and black[x+1] then
                dec(ans)
            else if not black[x-1] and not black[x+1] then
                inc(ans);

        end;
        black[x] := not black[x];
        writeln(ans);
    end;
    readln;
end.
