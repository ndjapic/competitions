program D_Forbidden_Difference;
uses
    math;
const
    dd = 1000 * 1000;
var
    n, d, i, x, ans: int32;
    c, dp_keep, dp_eras, dp: array [0 .. dd] of int32;

begin
    readln(n, d);

    for x := 0 to dd do c[x] := 0;

    for i := 1 to n do begin
        read(x);
        inc(c[x]);
    end;
    readln;

    ans := 0;
    if d = 0 then begin

        for x := 0 to dd do
            if c[x] > 0 then inc(ans, c[x] - 1);

    end else begin

        for x := 0 to dd do begin
            if (c[x] = 0) or (x-d < 0) or (c[x-d] = 0) then begin
                dp_keep[x] := 0;
                dp_eras[x] := c[x];
            end else begin
                dp_keep[x] := dp_eras[x-d];
                dp_eras[x] := c[x] + dp[x-d];
            end;
            dp[x] := min(dp_keep[x], dp_eras[x]);
        end;

        for x := d to dd do
            if (c[x-d] > 0) and (c[x] > 0) and ((x+d > dd) or (c[x+d] = 0)) then
                inc(ans, dp[x]);

    end;

    writeln(ans);
end.
