# Задатак: B_Gellyfish_and_Baby_s_Breath.pas

```pascal
program B_Gellyfish_and_Baby_s_Breath;
uses
    math;
const
    nn = 100 * 1000;
    prime = 998244353;
var
    ntc, tci, n, i, j, k: int32;
    p, q, r, pow2: array [0 .. nn] of int32;

begin
    pow2[0] := 1;
    for i := 1 to nn do begin
        pow2[i] := pow2[i-1] * 2;
        if pow2[i] >= prime then dec(pow2[i], prime);
    end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 0 to n-1 do read(p[i]); readln;
        for j := 0 to n-1 do read(q[j]); readln;

        i := 0;
        j := 0;
        for k := 0 to n-1 do begin

            if p[k] > p[i] then i := k;
            if q[k] > q[j] then j := k;

            if p[i] > q[j] then begin
                r[k] := pow2[p[i]] + pow2[q[k-i]];
                {writeln('1 ', k, ' ', i, ' ', j);}
            end else if q[j] > p[i] then begin
                r[k] := pow2[q[j]] + pow2[p[k-j]];
                {writeln('2 ', k, ' ', i, ' ', j);}
            end else if p[k-j] > q[k-i] then begin
                r[k] := pow2[q[j]] + pow2[p[k-j]];
                {writeln('3 ', k, ' ', i, ' ', j);}
            end else begin
                r[k] := pow2[p[i]] + pow2[q[k-i]];
                {writeln('4 ', k, ' ', i, ' ', j);}
            end;

            if r[k] >= prime then dec(r[k], prime);

        end;

        for k := 0 to n-2 do write(r[k], ' ');
        writeln(r[n-1]);

    end;
end.

```
