program B_Arithmetic_Progression_Subsequence;
uses
    math;
const
    maxn = 100 * 1000;
var
    n, i, k: int32;
    ans: int64;
    c: array [0 .. maxn] of array [1 .. 10] of int32;
    mx: array [1 .. maxn] of int32;
    ai, aj, ak: int8;

begin
    readln(n);

    for ai := 1 to 10 do c[0][ai] := 0;

    for i := 1 to n do begin
        c[i] := c[i-1];
        read(ai);
        inc(c[i][ai]);
        mx[i] := 0;
    end;
    readln;

    for ai := 1 to 10 do
        if c[n][ai] > 0 then begin

            ak := 2 - ai mod 2;
            while ak <= 10 do begin
                if c[n][ak] > 0 then begin

                    aj := (ai+ak) div 2;
                    i := 1;
                    k := i+2;

                    while k <= n do
                        if c[i][ai] = c[i-1][ai] then
                            inc(i)
                        else if (k-i < 2) or (c[k][ak] = c[k-1][ak]) or (c[k-1][aj] = c[i][aj]) then
                            inc(k)
                        else begin
                            mx[k] := max(mx[k], i);
                            inc(i);
                        end;

                end;
                inc(ak, 2);
            end;

        end;

    ans := 0;
    for k := 3 to n do begin
        mx[k] := max(mx[k], mx[k-1]);
        inc(ans, int64(mx[k] - mx[k-1]) * (n-k+1));
    end;

    writeln(ans);
end.
