program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
var
	n, h, i: int8;
	i1, i2: int32;
	k, a, b, x1, x2: int64;
	found: boolean;
	d1, d2: tdictionary<int64, boolean>;
	keys1, keys2: TList<int64>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);
	h := n div 2;

	d1 := tdictionary<int64, boolean>.create;
	d2 := tdictionary<int64, boolean>.create;
	d1.add(0, true);
	d2.add(0, true);
	keys1 := tlist<int64>.create;
	keys2 := tlist<int64>.create;

	for i := 1 to h do begin
		readln(a, b);

		keys1.clear;
		for x1 in d1.keys do keys1.add(x1);

		for x1 in keys1 do
			if x1 + a <= k then
				d1.addorsetvalue(x1 + a, true);

		for x1 in keys1 do
			if x1 + b <= k then
				d1.addorsetvalue(x1 + b, true);
	end;

	for i := h + 1 to n do begin
		readln(a, b);

		keys2.clear;
		for x2 in d2.keys do keys2.add(x2);

		for x2 in keys2 do
			if x2 + a <= k then
				d2.addorsetvalue(x2 + a, true);

		for x2 in keys2 do
			if x2 + b <= k then
				d2.addorsetvalue(x2 + b, true);
	end;

	keys1.clear;
	keys2.clear;
	for x1 in d1.keys do keys1.add(x1);
	for x2 in d2.keys do keys2.add(x2);
	keys1.sort;
	keys2.sort;
	i1 := 0;
	i2 := keys2.count - 1;
	found := false;

	while (i1 < keys1.count) and (i2 >= 0) and not found do
		if keys1[i1] + keys2[i2] < k then
			inc(i1)
		else if keys1[i1] + keys2[i2] > k then
			dec(i2)
		else
			found := true;

	if found then
		writeln('Yes')
	else
		writeln('No');

	d1.free;
	d2.free;
	keys1.free;
	keys2.free;
end.
