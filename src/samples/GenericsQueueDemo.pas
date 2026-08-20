program GenericsQueueDemo;
{$mode delphi}
uses
	Generics.Collections{, SysUtils};
var
	RedPoruka: TQueue<string>;
	TrenutnaPoruka: string;

begin
	// 1. Иницијализација реда
	RedPoruka := TQueue<string>.Create;
	try
		writeln('--- Додавање елемената у ред ---');
		// 2. Додавање елемената на крај реда помоћу Enqueue
		RedPoruka.Enqueue('Прва порука');
		RedPoruka.Enqueue('Друга порука');
		RedPoruka.Enqueue('Трећа порука');

		// Приказ тренутног броја елемената
		writeln('Број елемената у реду: ', RedPoruka.Count); // Исписује 3

		writeln(#10'--- Читање без уклањања ---');
		// 3. Преглед првог следећег елемента помоћу Peek
		if RedPoruka.Count > 0 then
			writeln('Следећи на реду за процесирање (Peek): ', RedPoruka.Peek);

		writeln(#10'--- Обрада елемената (Уклањање) ---');
		// 4. Уклањање елемената са почетка реда помоћу Dequeue
		while RedPoruka.Count > 0 do begin
			TrenutnaPoruka := RedPoruka.Dequeue;
			writeln('Процесирано: ', TrenutnaPoruka);
		end;

		// Провера да ли је ред празан
		writeln('Број елемената након обраде: ', RedPoruka.Count); // Исписује 0

	finally
		// 5. Ослобађање меморије
		RedPoruka.Free;
	end;

	readln;
end.
