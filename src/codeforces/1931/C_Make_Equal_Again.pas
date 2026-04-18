program C_Make_Equal_Again;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, l, r, price1, price2: int32;
    a: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]);
        readln;

        l := 1;
        r := n;
        while (l <= r) and (a[l] = a[1]) do inc(l);
        while (l <= r) and (a[r] = a[1]) do dec(r);
        price1 := r-l+1;

        l := 1;
        r := n;
        while (l <= r) and (a[l] = a[n]) do inc(l);
        while (l <= r) and (a[r] = a[n]) do dec(r);
        price2 := r-l+1;

        writeln(min(price1, price2));

    end;
end.
