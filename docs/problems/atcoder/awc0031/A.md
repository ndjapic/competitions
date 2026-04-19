# Problem: A.pas

```pascal
program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	Generics.Defaults, Generics.Collections;
const
	nn = 100 * 1000;
var
	n, m, k, i, j, o, ans: int32;
	p: TList<int32>;
	v, c: array [0 .. nn] of int32;
	t: array [0 .. nn] of array of int32;
	specialized: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function CompareChefs(constref Left, Right: int32): Integer;
begin
	Result := v[Right] - v[Left];
	if Result = 0 then
		Result := Left - Right;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, k);

	p := TList<int32>.Create;
	for i := 0 to n-1 do begin
		read(v[i]);
		read(c[i]);
		setlength(t[i], c[i]);
		for j := 0 to c[i] - 1 do read(t[i][j]);
		readln;
		p.Add(i);
	end;

	p.Sort(TComparer<int32>.Construct(CompareChefs));

	for j := 1 to m do specialized[j] := 0;

	for o := 0 to k-1 do begin
		i := p[o];
		for j := 0 to c[i] - 1 do inc(specialized[t[i][j]]);
	end;

	ans := 0;
	for j := 1 to m do
		if specialized[j] = k then inc(ans);

	writeln(ans);
	p.Free;
end.

```
