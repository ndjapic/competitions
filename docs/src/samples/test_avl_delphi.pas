program test_avl_delphi;

{$MODE DELPHI}

uses
	AVL_Tree, SysUtils, Math;

function CompareInt64(Data1, Data2: Pointer): int32;
begin
	Result := CompareValue(PInt64(Data1)^, PInt64(Data2)^);
end;

var
	Tree: TAVLTree;
	Node: TAVLTreeNode;
	P: PInt64;
	Vals: array of Int64;
	I: Integer;

begin
	Tree := TAVLTree.Create(CompareInt64);
	try
		// Primer sa nekim velikim brojevima (Int64)
		SetLength(Vals, 4);
		Vals[0] := 9223372036854775807; // Max Int64
		Vals[1] := 10;
		Vals[2] := -9223372036854775808; // Min Int64
		Vals[3] := 5000000000;					// Više od 32-bitnog Integera

		Writeln('Ubacujem Int64 vrednosti...');
		for I := 0 to High(Vals) do
		begin
			New(P);
			P^ := Vals[I];
			Tree.Add(P);
		end;

		Writeln('Sortirani Int64 izlaz:');
		Node := Tree.FindLowest;
		while Assigned(Node) do
		begin
			Writeln(PInt64(Node.Data)^);
			Node := Tree.FindSuccessor(Node);
		end;

	finally
		// Oslobađanje memorije za PInt64
		Node := Tree.FindLowest;
		while Assigned(Node) do
		begin
			Dispose(PInt64(Node.Data));
			Node := Tree.FindSuccessor(Node);
		end;
		Tree.Free;
	end;
end.
