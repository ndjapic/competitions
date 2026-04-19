# Problem: C_Inhabitant_of_the_Deep_Sea.pas

```pascal
program C_Inhabitant_of_the_Deep_Sea;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, l, r: int32;
    k, mn: int64;
    a: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        for i := 1 to n do read(a[i]); readln;

        l := 1;
        r := n;
        while (k > 0) and (l <= r) do
            if l = r then begin

                mn := min(a[l], k);

                dec(a[l], mn);
                dec(k, mn);

                if a[l] = 0 then inc(l);

            end else if k > 1 then begin

                mn := min(a[l], a[r]);
                mn := min(mn, k div 2);

                dec(a[l], mn);
                dec(a[r], mn);
                dec(k, mn*2);

                if a[l] = 0 then inc(l);
                if a[r] = 0 then dec(r);

            end else if k > 0 then begin

                dec(a[l]);
                dec(k);
                if a[l] = 0 then inc(l);

            end;

        writeln(n - (r-l+1));

    end;
end.

```
