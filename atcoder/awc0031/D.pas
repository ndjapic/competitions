program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections, Math;
var
	n, m, i, j, elm: int32;
	s: int64;
	ans: boolean;
	l, r: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	l := TList<int32>.Create;
	for i := 1 to n do begin
		read(elm);
		l.Add(elm);
	end;
	readln;
	l.Sort;

	r := TList<int32>.Create;
	for j := 1 to m do begin
		read(elm);
		r.Add(elm);
	end;
	readln;
	r.Sort;

	ans := true;
	s := 0;
	j := m-1;
	for i := 0 to n-1 do
		if ans then begin
			while (j >= 0) and (n-r[j] <= i) do dec(j);
			inc(s, j+1-m+l[i]);
			ans := s >= 0;
		end;

	if ans then begin
		writeln('Yes');
	end else
		writeln('No');

	l.Free;
	r.Free;
end.
