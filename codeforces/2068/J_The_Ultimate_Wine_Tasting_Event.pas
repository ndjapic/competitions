program J_The_Ultimate_Wine_Tasting_Event;
{$MODE DELPHI}
const
    nn = 100;
var
    ntc, tci: int16;
    n, i, c, h, ia, ib: int16;
    ans: boolean;
    s: string;
    a, b: array [1 .. nn] of int16;

procedure swp(i, j: int16);
var
    ch: char;
begin
    ch := s[i];
    s[i] := s[j];
    s[j] := ch;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);
		readln(s);

        c := 0;
        for i := 1 to n do
            if s[i] = 'W' then inc(c);

        ans := not odd(c);
        if ans then begin
            h := c div 2;
            ia := 0;
            ib := 0;

            c := 0;
            for i := 1 to n do begin
                if s[i] = 'W' then inc(c);
                if (s[i] = 'R') or (c <= h) then begin
                    inc(ia);
                    a[ia] := i;
                end else begin
                    inc(ib);
                    b[ib] := i;
                end;
            end;

            c := 0;
            for i := n+1 to 2*n do begin
                if s[i] = 'R' then inc(c);
                if (s[i] = 'W') or (c > h) then begin
                    inc(ib);
                    b[ib] := i;
                end else begin
                    inc(ia);
                    a[ia] := i;
                end;
            end;

            for i := 1 to n do swp(a[i], b[i]);

            i := 1;
            while (i <= n) and (s[i] = 'W') do inc(i);
            ans := i > n;

            if ans then begin
                while (i <= 2*n) and (s[i] = 'R') do inc(i);
                ans := i > 2*n;
            end;

        end;

        if ans then
            writeln('YES')
        else
            writeln('NO');

    end;
end.
