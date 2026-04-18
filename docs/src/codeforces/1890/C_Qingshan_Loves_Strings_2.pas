program C_Qingshan_Loves_Strings_2;
const
    maxn = 400;
var
    ntc, tci, n, i, i0, l, r, p: int16;
    a: array [-maxn .. maxn] of char;
    x: array [-maxn .. maxn] of int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln;

        p := 0;
        i0 := 0;
        l := 1;
        r := n;

        while (p > -1) and (p <= 300) and (l <= r) do
            if a[l] <> a[r] then begin
                inc(l);
                dec(r);
            end else if a[l] = '1' then begin
                inc(p);
                x[p] := l-1-i0;
                dec(l);
                a[l] := '1';
                dec(r);
                dec(i0, 2);
            end else if a[r] = '0' then begin
                inc(p);
                x[p] := r-i0;
                inc(r);
                a[r] := '0';
                inc(l);
            end;

        if p > 300 then p := -1;
        writeln(p);
        if p > -1 then begin
            for i := 1 to p-1 do write(x[i], ' ');
            if p > 0 then write(x[p]);
            writeln;
        end;

    end;
end.
