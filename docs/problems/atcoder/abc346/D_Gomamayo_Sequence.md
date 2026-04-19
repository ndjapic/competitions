# Problem: D_Gomamayo_Sequence.pas

```pascal
program D_Gomamayo_Sequence;
{$H+}
uses
    math;
const
    maxn = 200 * 1000 + 1;
var
    n, i: int32;
    ans: int64;
    s: string;
    c: array [1 .. maxn] of int32;
    pre, suf: array [0 .. maxn] of int64;

begin
    readln(n);
    readln(s);

    for i := 1 to n do begin
        read(c[i]);
        if odd(i) then begin
            if s[i] = '0' then
                s[i] := '1'
            else
                s[i] := '0'
        end;
    end;
    readln;

    ans := high(int64);

    (* 00...011...1 *)

    pre[0] := 0;
    for i := 1 to n do begin
        pre[i] := pre[i-1];
        if s[i] = '1' then inc(pre[i], c[i]);
    end;

    suf[n+1] := 0;
    for i := n downto 1 do begin
        suf[i] := suf[i+1];
        if s[i] = '0' then inc(suf[i], c[i]);
    end;

    for i := 1 to n-1 do
        ans := min(ans, pre[i] + suf[i+1]);

    (* 11...100...0 *)

    pre[0] := 0;
    for i := 1 to n do begin
        pre[i] := pre[i-1];
        if s[i] = '0' then inc(pre[i], c[i]);
    end;

    suf[n+1] := 0;
    for i := n downto 1 do begin
        suf[i] := suf[i+1];
        if s[i] = '1' then inc(suf[i], c[i]);
    end;

    for i := 1 to n-1 do
        ans := min(ans, pre[i] + suf[i+1]);

    writeln(ans);
end.

```
