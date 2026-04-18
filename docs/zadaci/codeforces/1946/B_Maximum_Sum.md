# Задатак: B_Maximum_Sum.pas

```pascal
program B_Maximum_Sum;
uses
    math;
const
    maxn = 200 * 1000;
    prime = 1000 * 1000 * 1000 + 7;
var
    ntc, tci: int16;
    n, k, i: int32;
    s, current_sum, maximum_sum: int64;
    a: array [1 .. maxn] of int32;
    p2: array [0 .. maxn] of int32;

begin
    p2[0] := 1;
    for k := 1 to maxn do begin
        p2[k] := p2[k-1] * 2;
        if p2[k] >= prime then dec(p2[k], prime);
    end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        s := 0;
        current_sum := 0;
        maximum_sum := 0;

        for i := 1 to n do begin
            read(a[i]);
            inc(s, a[i]);
            if s < 0 then inc(s, prime);
            current_sum := max(current_sum + a[i], 0);
            maximum_sum := max(maximum_sum, current_sum);
        end;
        readln;

        if maximum_sum < 0 then
            maximum_sum := prime - ((-maximum_sum) mod prime);
        s := s mod prime;
        maximum_sum := maximum_sum mod prime;

        dec(s, maximum_sum);
        if s < 0 then inc(s, prime);
        inc(s, maximum_sum * p2[k]);
        s := s mod prime;
        writeln(s);

    end;
end.

```
