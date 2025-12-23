program A_Yet_Another_AB_Problem;
{$H+}
const
    maxn = 200 * 1000;
var
    n, i, j, aa, ba, bb, ans: int32;
    s, t: string;
    link: array [1 .. maxn] of int32;

begin
    readln(n);
    readln(s);
    readln(t);

    aa := 0;
    ba := 0;
    bb := 0;
    ans := 0;

    for j := 1 to n do
        if ans > -1 then begin

            if (s[j] = 'A') and (t[j] = 'A') then begin
                link[j] := aa;
                aa := j;
            end else if (s[j] = 'A') and (t[j] = 'B') then begin

                inc(ans);
                link[j] := bb;
                bb := j;

                if ba > 0 then begin
                    i := link[ba];
                    ba := link[ba];
                    link[i] := aa;
                    aa := i;
                end else if aa > 0 then
                    (* aa := link[aa] *)
                else
                    ans := -1;

            end else if (s[j] = 'B') and (t[j] = 'A') then begin
                link[j] := ba;
                ba := j;
            end else if (s[j] = 'B') and (t[j] = 'B') then begin
                link[j] := bb;
                bb := j;
            end;

        end;

    while (ba > 0) and (ans > -1) do
        if ba < bb then begin
            inc(ans);
            bb := link[bb];
            ba := link[ba];
        end else
            ans := -1;

    writeln(ans);
end.
