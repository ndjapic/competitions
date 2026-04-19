# Problem: C_Beware_of_Overflow.pas

```pascal
program C_Beware_of_Overflow;
{$mode objfpc}
uses
    SysUtils;
const
    nn = 20000;
type
    EMy = class(Exception);
var
    n, i, j, t, l, r, m: int16;
    query: int16;
    p, q: array [1 .. nn] of int16;

function add(i, j: int16): int16;
begin
    writeln('+ ', i, ' ', j);
    flush(output);
    readln(result);
end;

function cmp(i, j: int16): int8;
begin
    writeln('? ', i, ' ', j);
    flush(output);
    readln(result);
end;

begin
    readln(n);

    try
        for t := 1 to n do begin

            l := 0;
            r := t;
            while r-l > 1 do begin
                m := (l+r) div 2;
                query := cmp(p[m], t);
                if query = -1 then raise EMy.Create('lel');
                if query = 1 then
                {if a[p[m]] < a[t] then}
                    l := m
                else
                    r := m;
            end;

            for i := 1 to t-1 do q[p[i]] := i;

            for j := 1 to t-1 do
                if q[j] >= r then inc(q[j]);

            q[t] := r;
            for j := 1 to t do p[q[j]] := j;

        end;

        while n > 1 do begin

            t := add(p[1], p[n]);
            if t = -1 then raise EMy.Create('lel');
            dec(n);

            l := 1;
            r := n+1;
            while r-l > 1 do begin
                m := (l+r) div 2;
                query := cmp(t, p[m]);
                if query = -1 then raise EMy.Create('lel');
                if query = 0 then
                    l := m
                else
                    r := m;
            end;

            for i := 2 to l do p[i-1] := p[i];
            p[l] := t;

        end;

        writeln('!');
        flush(output);
        {readln(query);
        if query = -1 then raise EMy.Create('lel');}
    except
        on e: EMy do begin
            ExitCode := 0;
        end;
    end;
end.

```
