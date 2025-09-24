---
title: 'Взаимосвязи в компании: отчет'
author: "Помазкова Арина, atpomazkova"
output: 
  html_document:
    code_folding: hide
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Загрузка данных

```{r message = FALSE, warning=FALSE, echo = F}
LDC = read.csv("~/shared/minor2_2022/2-tm-net/hw/LDC_topics.csv")

library(igraphdata)
library(igraph)
data(enron) 
library(lubridate)
source("~/shared/minor2_2022/2-tm-net/hw/personalTask.R")
start_date = hw_net_get_start_date()
# определяем конечную точку месяца
last_date = start_date + dmonths(1)
time = as_date(as_datetime(E(enron)$Time))
# убираем вершины раньше начальной точки и позже конечной
net = enron %>% delete_edges(E(enron)[time < start_date | time > last_date])
# убираем связи вершины самой с собой (согласно данным, иногда люди себя в копию ставят)
net = simplify(net, remove.multiple = F)
# убираем обособленные вершины
net = net %>% delete_vertices(V(net)[degree(net) == 0])
```

Анализируются данные с `r start_date` по `r last_date`

## Описание сети

Для начала визуализируем сеть, чтобы получить общее понимание о ней.

```{r message = FALSE, warning=FALSE, echo = F}
library(ggraph)

net_graph = ggraph(net) +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(aes(size = degree(net)/3, colour = V(net)$Note))  +
  theme_void()

net_graph +
  theme(legend.position="none")
```

Легенда была извлечена из графика намеренно, так как она занимала слишком много места и перекрывала сеть. Но даже визуально видно, что разные должности (отмеченные разным цветом) имеют тенденции занимать определённые места в сети.

```{r message = FALSE, warning=FALSE, echo = F}
#смотрим на легенду отдельно
library(cowplot)
library(gridExtra)
net_legend = ggraph(net) +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(aes(colour = V(net)$Note))  +
  theme_void()
plot(get_legend(net_legend))
```

Итак, мы имеем сеть с ярко выраженными сообществами, в которых более крупные узлы кажутся более связанными между собой. Также можно визуально выделить сообщество из двух узлов, не связанных с остальными вершинами.

### Выявление значимых вершин

**Использованные меры центральности: битвинность, степень узлов**

Теперь проанализируем сеть с точки зрения показателей центральностей. Как уже было сказано, кажется, что определённые должности имеют тенденции занимать схожие места в сети. Отфильтруем сеть по показателям степени узлов и битвинности и визуализируем её.

**Степень узлов**
```{r message = FALSE, warning=FALSE, echo = F}
library(tidygraph)

net_tbl = as_tbl_graph(net)

morphed = net_tbl %>% morph(to_subgraph, degree(net_tbl) > quantile(degree(net_tbl), 0.75))

morphed <- morphed %>%
  mutate(selected_node = TRUE) %>% 
  unmorph() %>% 
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>% 
  arrange(selected_node)

morphed %>%
  ggraph(layout = "kk") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(aes(size = degree(net)/3, colour = selected_node))  +
  theme_void() +
  theme(legend.position="none")
```

Казалось бы, отмеченными оказались и не самые крупные узлы. Скорее всего, такие вершины получают письма в копии и не занимают высокие должности, так как им направляют не так много писем.

**Битвинность**
```{r message = FALSE, warning=FALSE, echo = F}
morphed_b = net_tbl %>% morph(to_subgraph, betweenness(net_tbl) > quantile(betweenness(net_tbl), 0.75))

morphed_b <- morphed_b %>%
  mutate(selected_node = TRUE) %>% 
  unmorph() %>% 
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>% 
  arrange(selected_node)

morphed_b %>%
  ggraph(layout = "kk") +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(aes(size = degree(net)/3, colour = selected_node))  +
  theme_void() +
  theme(legend.position="none")

```

Здесь ситуация кажется более ясной. Очевидно, что узлы с более высокой битвинностью пропускают через себя больше писем и, соотвественно, связывают между собой сообщества.

### Выявление сообществ

**Использованные меры выделения сообществ: walktrap**

Так как у нас направленный граф, то для выделения сообществ возможно использовать ограниченное количество инструментов.

```{r message = FALSE, warning=FALSE, echo = F}
#находим
community = cluster_walktrap(net)
community

net = net %>% set.vertex.attribute('community', value = membership(community))
```

Всего получилось выделить 18 сообществ. Визуализируем их на сети.

```{r message = FALSE, warning=FALSE, echo = F}
  ggraph(net) +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(aes(size = degree(net)/3, color = as.factor(V(net)$community)))  +
  theme_void()
```

Примечательно, что эти загадочные два работника с единственной перепиской за месяц относятся к разным группам. Как было сказано раннее, более крупные узлы действительно относятся к одной группе. Проверим, насколько это можно проверить с помощью ассортативности.

### Исследовательские вопросы

**Вопрос 1: Сответствуют ли степени узлов их группам?**

Исходя из предыдущего рассуждения, я могу предположить, что вершины с похожими степенями состоят в одной группе.

```{r message = FALSE, warning=FALSE, echo = F}
assortativity(net, V(net)$community)
```
Показатель ассортативности очень высок, поэтому моя гипотеза подтверждается. Если рассуждать логически, то можно прийти к выводу, что такой результат мы получили из-за того, что люди общаются больше с теми, кто соответсвует их должности: например, собираются на совет директоров или устаивают собрание отдела.

**Вопрос 2: Правда ли, что вершины с самой большой битвинностью это менеджеры?**

Как уже было замечено ранее, вершины с самой большой битвинность находятся на пересечении сообществ. Если обратиться к самому первому графику с разделением должностей по цветам, то можно предположить, что именно менеджеры являются связующими элементами между группами.

```{r message = FALSE, warning=FALSE, echo = F}
#выделяем  менеджеров
library(stringr)

net = net %>% set.vertex.attribute('manager', value = str_detect(V(net)$Note, 'Manag'))

ggraph(net) +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(aes(size = degree(net)/3, colour = V(net)$manager))  +
  theme_void() +
  theme(legend.position="none")
```

Чисто визуально моя гипотеза немного похожа на правдивую, однако перепроверим эти догадки с помощью теста.

```{r message = FALSE, warning=FALSE, echo = F}
x = betweenness(net)
y = as.numeric(V(net)$manager)

cor.test(x,y)
```

Ну нет, так нет. Посмотрим, какие должности занимают сотрудники с самой высокой битвинностью.

```{r message = FALSE, warning=FALSE, echo = F}
library(ggplot2)

net = net %>% set.vertex.attribute('betweenness', value = betweenness(net))

library(dplyr)
data = data.frame(vertex.attributes(net))
data = data %>% group_by(Note) %>% summarise(m = mean(betweenness)) %>% arrange(-m)

data
```

Моя первоначальная гипотеза не подтвердилась, так как "связующими узлами" между группами оказались именно руководящие должности (хотя некоторые из них тоже называются менеджерами).

## Место сотрудника в сети

```{r message = FALSE, warning=FALSE, echo = F}
hw_net_get_vertex(net)
```

Посмотрим его конкретное место на общем графике сети.

```{r message = FALSE, warning=FALSE, echo = F}
ggraph(net) +
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(aes(size = degree(net)/3, colour = (V(net)$Name == "Jeffrey Shankman")))  +
  theme_void() +
  theme(legend.position="none")
```

Можно сказать, что у него достаточно много связей в сети, однако степень узла не очень большая. Попробуем посмотреть, насколько он похож на тех, с кем у него есть связи.

```{r message = FALSE, warning=FALSE, echo = F}
j = subgraph.edges(net, E(net)[inc(V(net)[Name == "Jeffrey Shankman"])])
assortativity(j, V(j)) 
```

Оказалось, что этот работник совсем  не похож на тех, с кем он имеет связи. Теперь сравним его степень со степенями связанных с ним сотрудников.

```{r message = FALSE, warning=FALSE, echo = F}
ggraph(j)+
  geom_edge_link(color = "grey", alpha = 0.7) + 
  geom_node_point(aes(size = degree(j), colour = (V(j)$Name == "Jeffrey Shankman")))  +
  theme_void()
```

Даже без дополнительных расчётов видно, что его степень явно отличается от тех, с кем у него есть связь, кроме одной вершины. Посмотрим, чем же они ещё похожи. Мне кажется, что они могут находится в одной группе.

```{r message = FALSE, warning=FALSE, echo = F}
j = j %>% set_vertex_attr('degree', value = degree(j))
degree(j) #243, 135

net = net %>% set_vertex_attr('degree', value = degree(net))

data_j = data.frame(vertex.attributes(j))

data_j %>% filter(degree == 243|degree == 135) %>% select(Name, Note, community)
```

Хоть эти два узла и в одной группе, как и предполагалось, о втором работнике мало что известно, многие данные пропущенные. Я могу предположить, что это помощник или заместитель выбранного работника (так как он занимает достаточно высокую должность), и поэтому он получает практически такое же количество писем, как и его начальник.

Теперь сравним меры центарльности этой вершины по сравнению со всеми остальными вершинами.

```{r message = FALSE, warning=FALSE, echo = F}
data = data.frame(vertex.attributes(net))
data %>% filter(Name == "Jeffrey Shankman") #425.2303

ggplot(data) +
  geom_histogram(aes(x = betweenness)) +
  geom_vline(aes(xintercept = 425.2303), color = 'red')
```
```{r message = FALSE, warning=FALSE, echo = F}
data = data.frame(vertex.attributes(net))
data %>% filter(Name == "Jeffrey Shankman") #243

ggplot(data) +
  geom_histogram(aes(x = degree)) +
  geom_vline(aes(xintercept = 243), color = 'red')
```

Очевидно, что показатель его степени и битвинности гораздо выше большинства работников (что скорее всего связано с его высокой должностью), поэтому можно утверждать, что через него проходит огромное количество писем (даже при рассмотрении одного месяца). 

## Общие выводы
Подводя итоги всему анализу, можно сказать, что проанализированные связи в данной компании весьма типичны, и почти все очевидные гипотезы о переписке сотрудников были подтверждены. Однако для меня так и останется загадкой переписка этих двух обособленных работников. Даже немного жаль, что мне не достался ни один из них.
