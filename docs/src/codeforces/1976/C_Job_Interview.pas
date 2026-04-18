program C_Job_Interview;
{$H+}
uses
    math;
const
    nn = 200 * 1000 + 1;
type
    tarr = array [1 .. nn] of int32;
var
    ntc, tci: int16;
    n, m, i, j, n1, m1, ioi: int32;
    iostr: string;
    skill: int64;
    a, b: tarr;

procedure readarr(var x: tarr);
var
    i, ioi: int32;
begin
    readln(iostr);
    setlength(iostr, length(iostr) + 1);
    iostr[length(iostr)] := ' ';
    ioi := 1;
    for i := 1 to n+m+1 do begin
        x[i] := 0;
        while (iostr[ioi] < '0') or (iostr[ioi] > '9') do inc(ioi);
        while (iostr[ioi] >= '0') and (iostr[ioi] <= '9') do begin
            x[i] := 10 * x[i] + ord(iostr[ioi]) - ord('0');
            inc(ioi);
        end;
    end;
end;

procedure writeint(x: int64);
begin
    if x > 0 then begin
        writeint(x div 10);
        inc(ioi);
        iostr[ioi] := chr(ord('0') + x mod 10);
    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);
        readarr(a);
        readarr(b);

        ioi := 0;
        setlength(iostr, 16*(n+m));
        for i := 1 to n+m+1 do begin

            n1 := 0;
            m1 := 0;
            skill := 0;
            for j := 1 to n+m+1 do
                if j <> i then begin

                    if (m1 = m) or (n1 < n) and (a[j] > b[j]) then begin
                        inc(n1);
                        inc(skill, a[j]);
                    end else begin
                        inc(m1);
                        inc(skill, b[j]);
                    end;

                end;

            writeint(skill);
            inc(ioi);
            iostr[ioi] := ' ';

        end;

        setlength(iostr, ioi-1);
        writeln(iostr);

    end;
end.
