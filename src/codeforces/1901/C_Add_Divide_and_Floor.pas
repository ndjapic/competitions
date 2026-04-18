program C_Add_Divide_and_Floor;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, mn, mx, ans: int32;
    a: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        mn := high(int32);
        mx := 0;
        for i := 1 to n do begin
            read(a[i]);
            mn := min(mn, a[i]);
            mx := max(mx, a[i]);
        end;

        ans := 0;
        while mx > mn do begin
            mx := (mn + mx) div 2;
            inc(ans);
        end;

        writeln(ans);
        if (0 < ans) and (ans <= n) then begin
            for i := 1 to ans-1 do write(mn, ' ');
            writeln(mn);
        end;

    end;
end.
