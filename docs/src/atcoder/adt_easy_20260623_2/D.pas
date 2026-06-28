program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #counter #keys #sort
uses
	Generics.Collections,
	Generics.Defaults, Math;
var
	n, k, i, x, c: int8;
	s, t: string;
	d: tdictionary<string, int8>;
	Keys: TList<string>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	readln(s);

	x := 0;
	d := tdictionary<string, int8>.create;
	for i := 1 to n-k+1 do begin
		t := copy(s, i, k);
		if not d.TryGetValue(t, c) then c := 0;
		inc(c);
		d.AddOrSetValue(t, c);
		x := max(x, c);
	end;

	Keys := TList<string>.Create;
	for t in d.Keys do
		if d.TryGetValue(t, c) and (c = x) then Keys.Add(t);
	Keys.Sort;

	writeln(x);
	for i := 0 to Keys.Count - 1 do begin
		Write(Keys[i]);
		if i < Keys.Count - 1 then Write(' ');
	end;
	WriteLn;

	d.Free;
	Keys.Free;
end.
