program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Collections, Generics.Defaults;
const
	nn = 200 * 1000;
var
	n, m, i, j, x, ans: int32;
	c, r: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	c := TList<int32>.Create;
	for i := 1 to n do begin
		read(x);
		c.Add(x);
	end;
	readln;
	c.Sort;

	r := TList<int32>.Create;
	for j := 1 to m do begin
		read(x);
		r.Add(x);
	end;
	readln;
	r.Sort;

	ans := 0;
	j := 0;
	for x in c do begin
		while (j < m) and (r[j] < x) do inc(j);
		if j < m then begin
			inc(ans);
			inc(j);
		end;
	end;

	c.Free;
	r.Free;
	writeln(ans);
end.
