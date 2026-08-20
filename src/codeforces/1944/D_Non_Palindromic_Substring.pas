program D_Non_Palindromic_Substring;
{H+}
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, q, i, l, r, d, h: int32;
    ans: int64;
    s: string;
    x: char;
    c1: array [0 .. maxn] of array ['a' .. 'z'] of int32;
    c2: array [-1 .. maxn] of array ['a' .. 'z'] of int32;

function nc2(n: int32): int64;
begin
    nc2 := int64(n-1) * n div 2;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, q);
        readln(s);

        for x := 'a' to 'z' do begin
            c1[0][x] := 0;
            c2[-1][x] := 0;
            c2[0][x] := 0;
        end;

        for i := 1 to n do begin
            c1[i] := c1[i-1];
            c2[i] := c2[i-2];
            inc(c1[i][s[i]]);
            inc(c2[i][s[i]]);
        end;

        for i := 1 to q do begin

            readln(l, r);

            d := r-l+1;
            h := d div 2;

            if c1[r][s[r]] - c1[l-1][s[r]] = d then
                ans := 0
            else if odd(d) then begin

                ans := nc2(d+1) - 1;
                if (c2[r][s[r]] - c2[l-2][s[r]] = h+1) and (c2[r-1][s[r-1]] - c2[l-1][s[r-1]] = h) then
                    ans := 2 * nc2(h+1);

            end else begin

                ans := int64(d+1) * d div 2 - 1;
                if (c2[r][s[r]] - c2[l-1][s[r]] = h) and (c2[r-1][s[r-1]] - c2[l-2][s[r-1]] = h) then
                    ans := 2 * nc2(h+1);

            end;

            writeln(ans);

        end;

    end;
end.
