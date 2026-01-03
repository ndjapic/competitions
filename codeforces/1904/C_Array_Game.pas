program C_Array_Game;
uses
    math;
const
    maxn = 20 * 2000;
var
    ntc, tci, n, k, i: int32;
    ans, need: int64;
    a, merge: array [1 .. maxn] of int64;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m+1, r);

        il := l;
        ir := m+1;
        for i := l to r do
            if (ir > r) or (il <= m) and (a[il] <= a[ir]) then begin
                merge[i] := a[il];
                inc(il);
            end else begin
                merge[i] := a[ir];
                inc(ir);
            end;

        for i := l to r do a[i] := merge[i];

    end;
end;

function gcd(a, b: int64): int64;
begin
    if b = 0 then
        gcd := a
    else begin
        inc(need, a div b);
        gcd := gcd(b, a mod b);
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        ans := 0;
        need := 0;
        for i := 1 to n do begin
            read(a[i]);
            ans := gcd(ans, a[i]);
        end;
        readln;

        if k < 10*n then begin
            while k > 0 do begin

                msort(1, n);
                inc(n);
                a[n] := high(int64);
                for i := 2 to n-1 do a[n] := min(a[n], a[i] - a[i-1]);
                dec(k);

            end;
            ans := a[n];
        end;

        writeln(ans);

    end;
end.
