# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	ntc, tci, n, i, ta, tb: int32;
	a, b, sa, sb: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(ntc);
	for tci := 1 to ntc do begin

		readln(a);
		n := length(a);
		setlength(sa, n);

		ta := 0;
		for i := 1 to n do begin
			inc(ta);
			sa[ta] := a[i];
			if (ta >= 4) and (copy(sa, ta - 3, 4) = '(xx)') then begin
				sa[ta - 3] := 'x';
				dec(ta, 2);
			end;
		end;
		setlength(sa, ta);

		readln(b);
		n := length(b);
		setlength(sb, n);

		tb := 0;
		for i := 1 to n do begin
			inc(tb);
			sb[tb] := b[i];
			if (tb >= 4) and (copy(sb, tb - 3, 4) = '(xx)') then begin
				sb[tb - 3] := 'x';
				dec(tb, 2);
			end;
		end;
		setlength(sb, tb);

		if ta = tb then begin
			i := 1;
			while (i <= ta) and (sa[i] = sb[i]) do inc(i);
			if i > ta then
				writeln('Yes')
			else
				writeln('No');
		end else
			writeln('No');

	end;
end.

```
