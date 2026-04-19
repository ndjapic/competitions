# Problem: B_YetnotherrokenKeoard.pas

```pascal
program B_YetnotherrokenKeoard; {$H+}
const
    maxn = 1000 * 1000;
var
    ntc, tci: int16;
    n, i, nl, nu, m: int32;
    s, t: string;
    active: array [1 .. maxn] of boolean;
    l, u: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s);
        n := length(s);
        nl := 0;
        nu := 0;

        for i := 1 to n do begin

            active[i] := false;

            if s[i] = 'b' then begin
                if nl > 0 then dec(nl);
            end else if s[i] = 'B' then begin
                if nu > 0 then dec(nu);
            end else if ('a' <= s[i]) and (s[i] <= 'z') then begin
                inc(nl);
                l[nl] := i;
            end else if ('A' <= s[i]) and (s[i] <= 'Z') then begin
                inc(nu);
                u[nu] := i;
            end;

        end;

        for i := 1 to nl do active[l[i]] := true;
        for i := 1 to nu do active[u[i]] := true;

        setlength(t, nl + nu);
        m := 0;

        for i := 1 to n do
            if active[i] then begin
                inc(m);
                t[m]:= s[i];
            end;

        writeln(t);

    end;
end.

```
