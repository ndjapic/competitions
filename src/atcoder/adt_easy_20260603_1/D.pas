program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #custom #sort
uses
	Generics.Collections,
	Generics.Defaults, Math;
const
	NN = 100;
var
	n, i: int8;
	ch: char;
	s: string;
	w: array [1 .. NN] of int8;
	p: TList<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function cmp(constref l, r: int8): int32;
begin
	Result := CompareValue(w[r], w[l]);
	if Result = 0 then
		Result := CompareValue(l, r);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n);

	p := TList<int8>.Create;
	for i := 1 to n do begin
		readln(s);

		w[i] := 0;
		for ch in s do
			if ch = 'o' then inc(w[i]);

		p.Add(i);
		p.Exchange(i-1, Random(i));
	end;
	p.Sort(TComparer<int8>.Construct(cmp));

	for i := 0 to n-1 do begin
		write(p[i]);
		if i < n-1 then write(' ');
	end;
	writeln;

	p.Free;
end.
