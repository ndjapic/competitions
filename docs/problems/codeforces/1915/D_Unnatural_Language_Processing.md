# Problem: D_Unnatural_Language_Processing.pas

```pascal
program D_Unnatural_Language_Processing;
{$H+}
const
    max2n = 300 * 1000 + 2;
var
    ntc, tci: int8;
    n, i, j: int32;
    s, t: string;

begin
    setlength(t, max2n);

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        i := n;
        j := 1;
        while i > 0 do
            if (s[i] = 'a') or (s[i] = 'e') then begin
                t[j] := s[i];
                t[j+1] := s[i-1];
                t[j+2] := '.';
                dec(i, 2);
                inc(j, 3);
            end else begin
                t[j] := s[i];
                t[j+1] := s[i-1];
                t[j+2] := s[i-2];
                t[j+3] := '.';
                dec(i, 3);
                inc(j, 4);
            end;

        dec(j, 2);
        setlength(s, j);
        for i := 1 to j do
            s[i] := t[j+1-i];

        writeln(s);

    end;
end.

```
