program C2_Magnitude;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
    prime = 998244353;
var
    ntc, tci: int16;
    n, i, ans: int32;
    s1, s2, s3, s4: int64;
    a: array [1 .. nn] of int32;
    mn, mx: array [0 .. nn] of int64;
    dp1, dp2: array [0 .. nn] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        mn[0] := 0;
        mx[0] := 0;
        dp1[0] := 1;
        dp2[0] := 1;

        for i := 1 to n do begin

            read(a[i]);

            s1 := mn[i-1] + a[i];
            s2 := mx[i-1] + a[i];
            s3 := abs(s1);
            s4 := abs(s2);

            mn[i] := s1;
            mx[i] := max(s3, s4);

            dp1[i] := dp1[i-1];
            dp2[i] := 0;

            if s3 = mn[i] then dp1[i] := (dp1[i] + dp1[i-1]) mod prime;

            if mn[i-1] <> mx[i-1] then begin
                if s1 = mx[i] then dp2[i] := (dp2[i] + dp1[i-1]) mod prime;
                if s3 = mx[i] then dp2[i] := (dp2[i] + dp1[i-1]) mod prime;
            end;
            if s2 = mx[i] then dp2[i] := (dp2[i] + dp2[i-1]) mod prime;
            if s4 = mx[i] then dp2[i] := (dp2[i] + dp2[i-1]) mod prime;

        end;
        readln;

        ans := dp2[n];

        writeln(ans);

    end;
end.
