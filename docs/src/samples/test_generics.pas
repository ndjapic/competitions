program test_avl;
{$mode objfpc}{$H+}
uses
	// Jedinica se zove AVL_Tree (sa donjom crtom)
	AVL_Tree, SysUtils;
type
	TMyData = record
		ID: Integer;
	end;
	PMyData = ^TMyData;

// Funkcija za poređenje (mora vraćati 0 ako su jednaki)
function CompareData(Data1, Data2: Pointer): integer;
begin
	Result := PMyData(Data1)^.ID - PMyData(Data2)^.ID;
end;

var
	Tree: TAVLTree;
	Node: TAVLTreeNode;
	Data: PMyData;
	I: Integer;

begin
	// Prosleđujemo funkciju za poređenje u konstruktor
	Tree := TAVLTree.Create(@CompareData);
	try
		Writeln('Ubacivanje elemenata: 50, 10, 30, 20');
		
		for I in [50, 10, 30, 20] do
		begin
			New(Data);
			Data^.ID := I;
			Tree.Add(Data);
		end;

		Writeln('Sortirani izlaz iz stabla:');
		Node := Tree.FindLowest;
		while Node <> nil do
		begin
			Write(PMyData(Node.Data)^.ID, ' ');
			Node := Tree.FindSuccessor(Node);
		end;
		Writeln;

	finally
		// Oslobađanje memorije
		Node := Tree.FindLowest;
		while Node <> nil do
		begin
			Dispose(PMyData(Node.Data));
			Node := Tree.FindSuccessor(Node);
		end;
		Tree.Free;
	end;
end.
