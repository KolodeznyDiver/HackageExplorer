# HackageExplorer
hackage.haskell.org packages scaner and local explorer.

## Зачем оно мне понадобилось?

Разбираться с пакетами на hackage / stackage не сильно удобно. В смысле, не разбираться с конкретным пакетом, а выбирать подходящий, сравнивать.
HackageExplorer скачивает описания пакетов с hackage в локальную базу. Инфу выдираю парся http://hackage.haskell.org/package/ИмяПакета. 

В базе:
  * название пакета;
  * краткое описание из заголовка;
  * описание в HTML сделанное автором пакета (с этим пришлось помучится, сочинить исправлялку, а то никакие парсеры не брали);
  * последняя версия пакета;
  * пакеты от которых пакет зависит;
  * автор(ы);
  * категории данные автором;
  * категории которые назначим мы сами;
  * адреса home page, репозиториев;
  * дата последнего обновления;
  * адрес пакета на Stackage (если есть) и версия там;
  * сколько скачивали за последний месяц;
  * есть ли доки на  hackage.
  * наши комменты про пакет.
	
После скачки описания пакетов можно выбирать пакеты по критериям:
  * дата последнего обновления, не ранее;
  * кол-во скачиваний за месяц, не менее;
  * есть ли пакет на Stackage;
  * ну и самое главное, по категориям (через and), но по категориям не назначенных пакету автором, а наш (хотя после загрузки описания нового пакета авторский список категорий совпадает с редактируемым). В этом суть. Я назначаю всякие свои подкатегории и могу, как мне хочется, пакеты сортировать.
	
К пакетам можно дописывать свои комменты. Пока однострочные. Нужно будет какой ни будь хтмыльный элемент для навороченного редактирования прикрутить. Было бы время на это.
Web-интерфейс, ибо сделано с помощью `stack new HackageExplorer yesod-sqlite`.
