program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort #set
uses
	SysUtils, Generics.Collections, Generics.Defaults, Math;
const
	NN = 200 * 1000;
var
	n, q, i, j, k, x: int32;
	tp: int8;
	box: array [1 .. NN] of TList<int32>;
	card: array [1 .. NN] of TDictionary<int32, boolean>;
	sorter: TList<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(n);
	readln(q);

	for j := 1 to n do
		box[j] := TList<int32>.Create;

	for i := 1 to NN do
		card[i] := TDictionary<int32, boolean>.Create;

	sorter := TList<int32>.Create;

	for k := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				read(i, j);
				box[j].Add(i);
				// box[j].Exchange(box[j].Count - 1, Random(box[j].Count));
				card[i].AddOrSetValue(j, true);
			end;

			2: begin
				read(j);
				if box[j].Count > 0 then begin
					box[j].Sort;
					write(box[j][0]);
					for x := 1 to box[j].Count - 1 do
						write(' ', box[j][x]);
				end;
				writeln;
			end;

			3: begin
				read(i);
				if card[i].Count > 0 then begin
					sorter.Clear;
					for x in card[i].Keys do begin
						sorter.Add(x);
						// sorter.Exchange(sorter.Count - 1, Random(sorter.Count));
					end;
					sorter.Sort;

					write(sorter[0]);
					for x := 1 to sorter.Count - 1 do
						write(' ', sorter[x]);
				end;
				writeln;
			end;

		end;
		readln;
	end;

	for j := 1 to n do box[j].Free;
	for i := 1 to NN do card[i].Free;
	sorter.Free;
end.
