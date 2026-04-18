program B_SUMdamental_Decomposition;
var
    ntc, tci: int16;
    n, x, ans: int32;

function c1(x: int32): int8;
begin
    if x = 0 then
        c1 := 0
    else
        c1 := c1(x div 2) + x mod 2;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n, x);

        if x > 1 then begin

            ans := x;
            dec(n, c1(x));
            if n > 0 then
                inc(ans, (n+1) div 2 * 2);

        end else if x = 1 then begin

            if odd(n) then
                ans := n
            else
                ans := n+3;

        end else if n = 1 then
            ans := -1
        else if odd(n) then
            ans := n+3
        else
            ans := n;

        writeln(ans);

    end;
end.
