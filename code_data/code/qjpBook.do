/*
- Title: Stata Basic Command Practice
- Author: LYU HAO
- Stata Version: 17.0
*/

* Research folder
global path D:\stata学习\因果推断实用计量方法\data

clear all
set more off

* 第三章 回归方法 stata命令实例

	*- 数据处理
	cd "$path/raw/chapter_03"
	use nsw_dw.dta, clear
	//obs 445
	//Source: https://users.nber.org/~rdehejia/nswdata2.html
	keep if treat == 1
	//保留 treated的样本, obs 185
	append using cps_controls.dta
	//obs 16177
	gen un74 = 0
	replace un74 = 1 if re74 == 0
	gen un75 = 0
	replace un75 = 1 if re75 == 0
	order re78, before(treat)
	//re78是被解释变量，treat是主要解释变量
	label var re78 "1978年收入"
	label var treat "是否参加培训(是, treat=1; 否, treat=0)"
	label var age "年龄"
	label var education "受教育年数"
	label var black "是否为非洲裔(是, black=1; 否, black=0)"
	label var hispanic "是否为西班牙裔(是, hispanic=1; 否, hispanic=0)"
	label var married "结婚否(是, married=1; 否, married=0)"
	label var nodegree "是否高中没毕业(是, nodegree=1; 否, nodegree=0)"
	label var re74 "1974年收入"
	label var re75 "1975年收入"
	label var un74 "1974年是否失业(是, un74=1; 否, un74=0)"
	label var un75 "1975年是否失业(是, un75=1; 否, un75=0)"
	cd "$path/final/chapter_03"
    save qjpBook_chap03.dta, replace

	*- 数据描述
	cd "$path/final/chapter_03"
	use qjpBook_chap03.dta, clear
	des re78 treat age education black hispanic married nodegree re74 re75 un74 un75
	sum re78 treat age education black hispanic married nodegree re74 re75 un74 un75
	
	*- 使用 tabstat 产生统计数据报告表格
	tabstat re78 treat age education black hispanic married nodegree re74 re75 un74 un75, ///
		statistics (n mean median min p25 p75 max) format(%-7.4g) column(statistics)
	/*
	- format x1 %10.3f ——将x1的列宽固定为10，小数点后取三位
	- format x1 %10.3g ——将x1的列宽固定为10，有效数字取三位
	- format x1 %10.3e ——将x1的列宽固定为10，采用科学计数法
	- format x1 %10.3fc ——将x1的列宽固定为10，小数点后取三位，加入千分位分隔符
	- format x1 %10.3gc ——将x1的列宽固定为10，有效数字取三位，加入千分位分隔符
	- format x1 %-10.3gc ——将x1的列宽固定为10，有效数字取三位，加入千分位分隔符，加入“-”表示左对齐

	- 常见的数据格式 %#.#g 与 %#.#f 的区别:
	  当指定小数点右边数字个数为0时，在不超过总位数的情况下，
	  g 仍可以显示原始数据中的小数；而 f 则仍默认不显示小数
	*/
	sum age, detail
	
	*- 变量之间相关性
	cor re78 treat age education black hispanic married nodegree re74 re75 un74 un75
	reg re78 treat age education black hispanic married nodegree re74 re75 un74 un75
	
	*- 检验两个系数是否相同
	test black = hispanic
	
	*- 检验系数共同显著性
	test married nodegree
	
	*- 储存回归结果
	reg re78 treat age black hispanic married nodegree re74 re75 un74 un75
	estimate store reg1, title(regression 1)
	reg re78 treat education black hispanic married nodegree re74 re75 un74 un75
	estimate store reg2, title(regression 2)

	*- 使用 esttab 报告结果
	esttab reg1 reg2, b(%7.3f) se(%7.3f) stat(N r2 F) title ("回归结果")

* 第四章 异方差、自相关、集群相关 stata命令实例

	*- 异方差
	
	*-- 产生模型数据
	clear
	quiet set obs 200
	//产生200个观测点
	gen x = 5*rnormal(0,1)
	//产生正态分布 N(0,5)的解释变量
	gen v = 5*rnormal(0,1)
	//产生正态分布 N(0,5)的干扰项
	gen w = exp(-0.5 + 0.2*x)
	//产生权重函数 w = exp(-0.5 + 0.2*x)
	gen e = w*v
	//产生异方差干扰项 e = w*v
	gen y = 5 + 3*x +e
	//产生解释变量 y = 5 + 3x + e
	
	*-- 使用OLS但忽略异方差
	reg y x
	
	*-- 使用OLS并考虑异方差
	reg y x, robust
	
	*-- 使用GLS
	gen one_star = 1/w
	gen y_star = y/w
	gen x_star = x/w
	
	reg y_star x_star one_star, noconstant
	
	*- 自相关
	
	*-- 产生模型数据
	clear
	quiet set obs 200
	//产生200个观测点
	gen time = _n
	//产生时间序列编号
	tsset time
	//设置为时间序列
	gen x = 0 in 1/1
	//X 的初始值为 0
	replace x = 0.4*l.x + rnormal(0,5) in 2/200
	gen v = rnormal(0,5)
	//产生正态分布 N(0,5) 的 v
	gen err = 0 in 1/1
	//干扰项初值 e(0) = 0
	scalar pho = 0.8
	//干扰项自相关系数 = 0.8
	replace e = pho*l.e + v in 2/200
	//e = pho*l.e + v
	gen y = 5 + 3*x +e
	//y = 5 + 3x + e
	
	*-- 使用OLS，但忽略自相关
	reg y x
	
	*-- 使用OLS并考虑异方差和自相关
	newey y x, lag(1)
	//使用 newey命令计算 HAC标准误差
	
	*-- 使用GLS
	gen one = 1
	gen one_star = one - pho*l.one
	gen y_star = y - pho*l.y
	gen x_star = x - pho*l.x
	reg y_star x_star one_star, noconstant
	
	*- 集群相关
	clear
	quiet set obs 30
	gen student = _n
	gen school = "M" in 1/3
	replace school = "T" in 4/6
	replace school = "Q" in 7/9
	replace school = "L" in 10/12
	replace school = "G" in 13/15
	replace school = "W" in 16/18
	replace school = "R" in 19/21
	replace school = "U" in 22/24
	replace school = "S" in 25/27
	replace school = "A" in 28/30
	egen score = fill(71(1)100)
	//生成71~100等差数列
	cd "$path/final/chapter_04"
    save qjpBook_chap04_clustering.dta, replace

	
	loneway score school
	//命令loneway计算集群内成绩相关系数
	sum score
	mean score
	//未意识到集群的存在，直接计算样本均值标准差和标准误差
	reg score
	//通过只有常数项的回归计算样本均值和方差
	reg score, cluster(school)
	
	/* stata集群相关方差命令
	- reg depvar indepvars, cluster(cluster identifier)
	- reg depvar indepvars, vce(cluster cluster identifier)
	*/

* 第五章 处置效应

	*- 随机分配实例：“田纳西学生/教师比例对学生学业成就影响”实验
	cd "$path/raw/chapter_05"
	import spss STAR_Students.sav, clear
	//Source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SIWH9F
	keep if FLAGSG1 == 1
	gen whiteasian = (race == 1 | race == 3)
	gen age1985 = 1985 - birthyear
	gen female = (gender == 2)
	gen score = g1treadss + g1tmathss
	keep stdntid g1schid whiteasian age1985 female g1classtype g1classsize score
	label var stdntid "学生编号"
	label var g1schid "学校编号"
	label var whiteasian "=1，白人和亚裔；=0，其他人种"
	label var age1985 "在1985年时的年龄"
	label var female "性别：=1，女；=0，男"
	label var g1classtype "班级类型：=1，小班；=2，普通班；=3，有辅导老师的加强普通班"
	label var g1classsize "班级人数"
	label var score "阅读和数学总成绩"
	cd "$path/final/chapter_05"
	save qjpBook_chap05.dta, replace

	des stdntid g1schid whiteasian age1985 female g1classtype g1classsize score
 	table g1classtype, stat(mean female whiteasian age1985 g1classsize)
 	gen small = (g1classtype == 1) 
 	gen regular = (g1classtype == 2)
 	gen regularaid = (g1classtype == 3)
 	destring score, replace
 	reg score small regularaid
 	table g1classtype, stat(mean score)
 	xi: reg score small regularaid i.g1schid
 	xi: reg score small regularaid female whiteasian age1985 i.g1schid
 	xi: reg score small regularaid female whiteasian age1985 i.g1schid, cluster(g1schid)

* 第六章 匹配方法：倾向得分匹配法实例

	*- 选择倾向得分模型，进行匹配前平衡性检验，考察共同支撑域
	cd "$path/final/chapter_06"
	use qjpBook_chap06.dta, clear
	tabstat age education black hispanic married nodegree re74 re75, ///
		statistics(mean) by(treat)
	pscore treat age education black hispanic married nodegree re74 re75, /// 
		un74 un75, logit comsup blockid(block) pscore(myscore)	
	/*
	- treat 处置变量
	- age~un75 控制变量
	- comsup 使用有共同支撑域样本
	- blockid(block) 分块变量
	- pscore(myscore) 储存倾向得分的变量
	*/
	
	cd "$path/final/chapter_06"
	use qjpBook_chap06.dta, clear
	gen age2 = age^2
	gen age3 = age^3
	gen education2 = education^2
	gen educationre74 = education*re74
	gen re742 = re74^2
	gen re752 = re75^2
	save v2_qjpBook_chap06.dta, replace

	pscore treat age age2 age3 education education2 black hispanic married nodegree ///
		re74 re742 re75 re752 un74 un75 educationre74, blockid(block) logit pscore(myscore)	
	//加入新变量，结果满足平衡条件
	
	*- 选择匹配模型，进行匹配后平衡性检验，计算处置效应
	
	*-- 使用 attnd 命令
	cd "$path/final/chapter_06"
	use v2_qjpBook_chap06.dta, clear
	pscore treat age age2 age3 education education2 black hispanic married nodegree ///
		re74 re742 re75 re752 un74 un75 educationre74, blockid(block) logit pscore(myscore)	
	
	attnd re78 treat, pscore(myscore)
	//没有使用 bootstrap 和 common support 选项
	attnd re78 treat, pscore(myscore) comsup
	//使用common support 选项
	attnd re78 treat, pscore(myscore) boot reps(100) comsup
	//使用 bootstrap 和 common support 选项
	
	*-- 使用 psmatch2 命令
	cd "$path/final/chapter_06"
	use v2_qjpBook_chap06.dta, clear
	pscore treat age age2 age3 education education2 black hispanic married nodegree ///
		re74 re742 re75 re752 un74 un75 educationre74, blockid(block) logit pscore(myscore)	
	
	psmatch2 treat, pscore(myscore) outcome(re78) neighbor(1) ties
	//没有使用 bootstrap 和 common support 选项
	psmatch2 treat, pscore(myscore) outcome(re78) neighbor(1) common ties
	// psmatch2 treat if comsup == 1, pscore(myscore) outcome(re78) neighbor(1) common ties
	//使用common support 选项
	bootstrap r(att), reps(100): psmatch2 treat age2 age3 education education2 black ///
		hispanic married nodegree re74 re742 re75 re752 un74 un75 educationre74, logit ///
		outcome(re78) neighbor(1) common ties
	//使用 bootstrap 和 common support 选项
	
	*-- 使用 teffects 命令
	cd "$path/final/chapter_06"
	use v2_qjpBook_chap06.dta, clear
	teffects psmatch (re78) (treat age age2 age3 education education2 black hispanic ///
		married nodegree re74 re742 re75 re752 un74 un75 educationre74, logit), ///\
		atet nneighbor(1)
	//没有使用 common support 选项

* 第七章 匹配方法与回归方法比较

	*- 数据处理
	cd "$path/raw/chapter_07"
	import excel using qjpBook_chap07_1.xlsx, firstrow clear
	cd "$path/final/chapter_07"
	save qjpBook_chap07_1.dta, replace

	*- 完全饱和模型
	cd "$path/final/chapter_07"
	use qjpBook_chap07_1.dta, clear
	gen D0AGE30 = (D == 0 & AGE == 30)
	gen D1AGE30 = (D == 1 & AGE == 30)
	gen D0AGE40 = (D == 0 & AGE == 40)
	gen D1AGE40 = (D == 1 & AGE == 40)
	list, noobs sepby(AGE)
	reg Y D0AGE30 D1AGE30 D0AGE40 D1AGE40, noconstant
	//不含常数项
	reg Y D1AGE30 D0AGE40 D1AGE40
	//含常数项
	gen AGE30 = (AGE == 30)
	gen AGE40 = (AGE == 40)
	reg Y D AGE30 AGE40, noconstant
	//控制变量饱和模型

	*- 缺乏共同支撑域和控制变量不均衡的影响
	cd "$path/raw/chapter_07"
	import excel using qjpBook_chap07_2.xlsx, firstrow clear
	drop ID
	rename INC1 inc1
	rename College college
	rename IQ iq
	cd "$path/final/chapter_07"
	save qjpBook_chap07_2.dta, replace
	reg inc1 college iq

	cd "$path/raw/chapter_07"
	import excel using qjpBook_chap07_3.xlsx, firstrow clear
	drop ID
	rename INC2 inc2
	rename College college
	rename IQ iq
	cd "$path/final/chapter_07"
	save qjpBook_chap07_3.dta, replace
	reg inc2 college iq

	cd "$path/raw/chapter_07"
	import excel using qjpBook_chap07_4.xlsx, firstrow clear
	drop ID
	rename INC1 inc1
	rename INC2 inc2
	rename College college
	rename IQ iq
	cd "$path/final/chapter_07"
	save qjpBook_chap07_4.dta, replace
	reg inc1 college iq
	reg inc2 college iq

* 第八章 面板数据分析方法

	/* 生成数据
	clear
	quiet set obs 32
	egen year = fill(2010(1)2017 2010(1)2017)
	bysort year: gen id = _n
	sort id year
	order id year
	gen tax = 0
	replace tax = 1 if id == 1 & year > 2013
	replace tax = 1 if id == 2 & year > 2013
	format q %9.2f
	//设置变量小数位数
	*/
	
	cd "$path/final/chapter_08"
	use qjpBook_chap08.dta, clear
	xtset id year
	xtsum q
	//分析企业业绩的变化来源
	
	*- 简单横截面回归
	reg q tax if year==2014
	
	*- 合并横截面回归
	reg q tax
	
	*- 固定效应模型LSDV估计
	cd "$path/final/chapter_08"
	use qjpBook_chap08.dta, clear
	xtset id year
	tabulate id, gen(firm)
	//根据 id 产生虚拟变量
	reg q tax firm1-firm4, noconstant
	//LSDV回归，noconstant设置没有共同截距项
	//4个虚拟变量的系数代表各个企业的个体固定效应
	
	*- 固定效应模型个体内估计
	
	*-- 手动命令
	cd "$path/final/chapter_08"
	use qjpBook_chap08.dta, clear
	xtset id year
	egen mq = mean(q), by(id)
	//按id，生成个体 q 均值
	egen mtax = mean(tax), by(id)
	//按id，生成个体 tax 均值
	gen within_q = q - mq
	//对 q 进行去个体均值转换
	gen within_tax = tax - mtax
	//对 tax 进行去个体均值转换
	reg within_q within_tax, noconstant
	
	*-- stata命令
	cd "$path/final/chapter_08"
	use qjpBook_chap08.dta, clear
	xtset id year
	xtreg q tax, fe

* 第九章 双重差分法

	*- 横截面单重差分
	cd "$path/final/chapter_09"
	use qjpBook_chap09.dta, clear
	xtset id year
	gen treat = id <= 2
	gen after = year >= 2014
	reg q treat if after == 1
	
	*- 时间序列单重差分
	cd "$path/final/chapter_09"
	use qjpBook_chap09.dta, clear
	xtset id year
	gen treat = id <= 2
	gen after = year >= 2014
	reg q after if treat == 1
	
	*- 基本双重差分法回归
	cd "$path/final/chapter_09"
	use qjpBook_chap09.dta, clear
	xtset id year
	gen treat = id <= 2
	gen after = year >= 2014
	gen treatafter = after*treat
	list id year q tax treat after treatafter, sep(8) ab(12) noobs
	//sep(8)   ab(12)   noobs
	reg q after treat treatafter
	
	*- 使用个体和时间固定效应
	cd "$path/final/chapter_09"
	use qjpBook_chap09.dta, clear
	xtset id year
	gen treat = id <= 2
	gen after = year >= 2014
	gen treatafter = after*treat
	tab id, gen(id)
	tab year, gen(year)
	reg q treatafter id1-id4 year2-year8, noconstant
	
	*- 细化研究事件对处置组在不同时间的影响
	cd "$path/final/chapter_09"
	use qjpBook_chap09.dta, clear
	xtset id year
	gen treat = id <= 2
	gen after1 = year == 2014
	gen after2 = year == 2015
	gen after3 = year > 2015
	gen treatafter1 = after1*treat
	gen treatafter2 = after2*treat
	gen treatafter3 = after3*treat
	tab id, gen(id)
	tab year, gen(year)
	reg q treatafter1 treatafter2 treatafter3 id1-id4 year2-year8, noconstant
	
	*- 双重差分法平行检验
	cd "$path/final/chapter_09"
	use qjpBook_chap09.dta, clear
	xtset id year
	gen treat = id <= 2
	gen after = year >= 2014
	gen treatafter = after*treat
	tab year, gen(year)
	gen treatyear2 = treat*year2
	gen treatyear3 = treat*year3
	gen treatyear4 = treat*year4
	reg q treat treatyear2-treatyear4 after treatafter

* 第十章 工具变量

	*- 处理数据
	cd "$path/raw/chapter_10"
	use maketable8.dta, clear
	keep if baseco == 1
	//保留基准样本
	keep logpgp95 avexpr lat_abst logem4 euro1900
	rename logpgp95 loggdp
	rename avexpr institutions
	rename lat_abst latitude
	rename logem4 logmortality
	cd "$path/final/chapter_10"
	save qjpBook_chap10.dta, replace
	
	*- OLS回归
	cd "$path/final/chapter_10"
	use qjpBook_chap10.dta, clear
	reg loggdp institutions latitude, robust
	
	*- 2SLS回归
	cd "$path/final/chapter_10"
	use qjpBook_chap10.dta, clear
	ivregress 2sls loggdp latitude (institutions = logmortality), first
	estat endogenous
	//Hausman检验解释变量是否是外生的
	estat firststage
	//检验工具变量是否是弱工具变量
	ivregress 2sls loggdp latitude (institutions = logmortality euro1900), first
	estat overid
	//过度识别情况下，检验工具变量是否是外生的

* 第十一章 样本自选择模型

	*- Heckman 样本选择模型
	
	*-- 手动估计模型
	cd "$path/final/chapter_11"
	use qjpBook_chap11.dta, clear
	sample 500, count
	//随机抽取500个观测点
	keep wage age education children
	list wage age education children if _n <=20
	
	gen work = (wage ~= .)
	//当 wage未缺失时，work = 1, 否则，work = 0
	probit work education age children
	//运用 Probit模型估计选择模型
	
	predict Z if e(sample), xb
	//用 Probit得到的系数计算 Z
	gen phi = normalden(Z)
	//计算对应的正态分布概率密度值
	gen PHI = normal(Z)
	//计算对应的正态分布的累计分布值
	gen lambda = phi/PHI
	//计算逆米尔斯比例
	list wage age education children Z phi PHI lambda if _n <= 20
	
	reg wage education age lambda
	//用计算出的逆米尔斯比例估计回归方程
	
	*-- 自动估计模型
	heckman wage education age, select(education age children) twostep
	//Heckman命令系数的方差比手动方法的系数方差小







