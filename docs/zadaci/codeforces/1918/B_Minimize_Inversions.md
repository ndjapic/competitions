# Задатак: B_Minimize_Inversions.pas

```pascal
program B_Minimize_Inversions;
{$H+}
const
	maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, ioi: int32;
    istr, ostr: string;
    a, b, p: array [1 .. maxn] of int32;

function readdword(): dword;
var
    ans: dword;
begin
    ans := 0;
    while (istr[ioi] < '0') or (istr[ioi] > '9') do inc(ioi);
    while (istr[ioi] >= '0') and (istr[ioi] <= '9') do begin
        ans := ans * 10 + ord(istr[ioi]) - ord('0');
        inc(ioi);
    end;
    readdword := ans;
end;

procedure writedword(x: dword);
begin
    if x >= 10 then writedword(x div 10);
    inc(ioi);
    ostr[ioi] := chr(x mod 10 + ord('0'));
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);

        readln(istr);
        istr := istr + ' ';
        ioi := 1;

        for i := 1 to n do a[i] := readdword();

        readln(istr);
        istr := istr + ' ';
        ioi := 1;

        for i := 1 to n do b[i] := readdword();

		for i := 1 to n do p[a[i]] := i;

        setlength(ostr, n*11);
        ioi := 0;

        for i := 1 to n do begin
            writedword(i);
            inc(ioi);
            ostr[ioi] := ' ';
        end;

        setlength(ostr, ioi-1);
        writeln(ostr);

        setlength(ostr, n*11);
        ioi := 0;

        for i := 1 to n do begin
            writedword(b[p[i]]);
            inc(ioi);
            ostr[ioi] := ' ';
        end;

        setlength(ostr, ioi-1);
        writeln(ostr);

    end;
end.

```
