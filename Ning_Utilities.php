<?php
use Aws\Ses\SesClient;

/**
* Ning Utilities
*/
class Ning_Utilities{
    private $_path;
    private $_webRoot;
    private $_AESKey;
	private $_timeZone;

	function __construct($contentType=''){
		if($contentType == 'json'){
			header("Content-Type: application/json; charset=UTF-8");
		}
	}
	private function GeoIP($IP=''){
		if($IP==''){$IP = $this -> getIP();}
		require_once ($this -> _path.'/bower/GeoIP2-php/vendor/autoload.php');
		try{
			$reader = new GeoIp2\Database\Reader($this -> _path.'/GeoLite2-City.mmdb');
			return $reader->city($IP);
		}
		catch(Exception $e) {
			return null;
		}
	}
	private function USPS($xml) {
		$ch = curl_init('http://production.shippingapis.com/ShippingAPI.dll');
		curl_setopt($ch, CURLOPT_POST, 1);
		curl_setopt($ch, CURLOPT_POSTFIELDS, $xml);
		curl_setopt($ch, CURLOPT_TIMEOUT, 60);
		curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
		curl_setopt($ch, CURLOPT_SSL_VERIFYPEER,false);
		$result = curl_exec($ch);
		$error = curl_error($ch);
		if(empty($error)) {
			return $result;
		}else{
			die(curl_error($ch));
		}
	}

    /**
     * Setter
     */
    public function setPath($path){
        $this -> _path = rtrim($path,'/');
    }
	public function setWebRoot($path){
		$this -> _webRoot = rtrim($path,'/');
	}
	public function setTimeZone($timeZone){
		$this -> _timeZone = $timeZone;
	}
    public function setAESKey($key){
        $this -> _AESKey = $key;
    }

    /**
     * Getter
     */
    public function getIP(){
		if(!empty($_SERVER['HTTP_CLIENT_IP'])){
			$user_ip = $_SERVER['HTTP_CLIENT_IP'];
		} 
		elseif(!empty($_SERVER['HTTP_X_FORWARDED_FOR'])){
			$user_ip = $_SERVER['HTTP_X_FORWARDED_FOR'];
		}
		else{
			$user_ip = $_SERVER['REMOTE_ADDR'];
		}
		return $user_ip;
	}
    public function getCountryIsoCode_GeoIP($IP=''){
        if($IP==''){$IP = $this -> getIP();}
        return strtolower($this -> GeoIP($IP)->country->isoCode);
    }
    public function getCountryName_GeoIP($IP=''){
        if($IP==''){$IP = $this -> getIP();}
        return $this->GeoIP($IP)->country->names['en'];
    }
    public function getCityName_GeoIP($IP=''){
        if($IP==''){$IP = $this -> getIP();}
        return $this->GeoIP($IP)->city->names['en'];
    }
    public function getVerifiedAddressFromUSPS($address1, $address2, $city, $state, $zipCode){
        $xml = 'API=Verify&XML= <AddressValidateRequest USERID="502ALLAN3802"><Address ID="1">';
        $xml .= '<Address1>' . $address1 . '</Address1>';
        $xml .= '<Address2>' . $address2 . '</Address2>';
        $xml .= '<City>' . $city . '</City>';
        $xml .= '<State>' . $state . '</State>';
        $xml .= '<Zip5>' . $zipCode . '</Zip5>';
        $xml .= '<Zip4></Zip4></Address>';
        $xml .= '</AddressValidateRequest>';
        $result = $this -> USPS($xml);
        $xml = new SimpleXMLElement($result);
        if(isset($xml->Address[0]->Error)||isset($xml->Address[1]->Error)) {
            return array('Error'=>'Address error');
        }
        return array('verifiedAddress1'=>(string)($xml->Address[0]->Address2),
            'verifiedAddress2'=>(string)($xml->Address[0]->Address1),
            'verifiedCity'=>(string)($xml->Address[0]->City),
            'verifiedState'=>(string)($xml->Address[0]->State),
            'verifiedZipCode'=>(string)($xml->Address[0]->Zip5 .'-' . $xml->Address[0]->Zip4));
    }
    public function getCityStateInfoFromUSPS($zipCode){
        $xml = 'API=CityStateLookup&XML= <CityStateLookupRequest USERID="502ALLAN3802"><ZipCode ID="0">';
        $xml .= '<Zip5>' . $zipCode . '</Zip5>';
        $xml .= '</ZipCode>';
        $xml .= '</CityStateLookupRequest>';
        $result = $this -> USPS($xml);
        $xml = new SimpleXMLElement($result);
        if(isset($xml->ZipCode[0]->Error)) {
            return array('Error'=>'ZipCode error');
        }
        return array('verifiedCity'=>(string)($xml->ZipCode[0]->City),
            'verifiedState'=>(string)($xml->ZipCode[0]->State));
    }
    public function getTimeZone($location_lat,$location_lng){
        return json_decode(file_get_contents("http://api.geonames.org/timezoneJSON?lat=$location_lat&lng=$location_lng&username=zhengning"));
    }
    public function getMicroTime(){
        date_default_timezone_set($this -> _timeZone);
        list($usec, $sec) = explode(' ', microtime());
        return ((float)$usec + (float)$sec);
    }
    public function getTimestamp(){
        date_default_timezone_set($this -> _timeZone);
        return time();
    }
    public function getAESEncrypt($content){
        if(!empty($this -> _AESKey) && !empty($content)){
            require_once ($this -> _path.'/AES.class.php');
            $AES = new CryptAES();
            $AES -> set_key($this -> _AESKey);
            $AES -> require_pkcs5();
            return $AES -> encrypt($content);
        }
        return null;
    }
    public function getAESDecrypt($content){
        if(!empty($this -> _AESKey) && !empty($content)){
            require_once ($this -> _path.'/AES.class.php');
            $AES = new CryptAES();
            $AES -> set_key($this -> _AESKey);
            $AES -> require_pkcs5();
            return $AES -> decrypt($content);
        }
        return null;
    }
    public function getJsonOutput($array,$prettyPrint=false){
        if($prettyPrint===true){
            return json_encode($array,JSON_UNESCAPED_UNICODE|JSON_PRETTY_PRINT|JSON_UNESCAPED_SLASHES);
        }
        return json_encode($array,JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES);
    }
    public function getRandomToken($length = 8,$type = ''){
        $token = "";
        $codeAlphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        $codeAlphabet .= "0123456789";
        if($type == 'int'){
            $codeAlphabet = "0123456789";
        }
        for($i = 0; $i < $length; $i ++){
            $token .= $codeAlphabet[$this -> cryptoRandSecure(0,strlen($codeAlphabet))];
        }
        return $token;
    }
    public function getCreditCardType($cardNumber){
        $cardNumber=preg_replace('/[^\d]/','',$cardNumber);
        if (preg_match('/^3[47][0-9]{13}$/',$cardNumber)) {
            $cardType = 'amex';
        }
        elseif (preg_match('/^3(?:0[0-5]|[68][0-9])[0-9]{11}$/',$cardNumber)) {
            $cardType = 'Diners Club';
        }
        elseif (preg_match('/^6(?:011|5[0-9][0-9])[0-9]{12}$/',$cardNumber)) {
            $cardType = 'discover';
        }
        elseif (preg_match('/^5[1-5][0-9]{14}$/',$cardNumber)) {
            $cardType = 'mastercard';
        }
        elseif (preg_match('/^4[0-9]{12}(?:[0-9]{3})?$/',$cardNumber)) {
            $cardType = 'visa';
        }
        return $cardType;
    }

    public function useMyLibs($items){
        $html = "\n";
        $_webRoot = $this -> _webRoot;
        if(isset($items['js'])){
            foreach ($items['js'] as $key => $item){
                reset($items['js']);
                if ($key === key($items['js'])){
                    $space = $item;
                    $html .= "$space<!-- myLibs JS -->".PHP_EOL;
                    continue;
                }
                switch ($item){
                    case 'jquery':
                        $html .= "$space<script src=\"$_webRoot/libs/myLibs/bower/jquery/dist/jquery.min.js\"></script>".PHP_EOL;
                        break;
                    case 'bootstrap':
                        $html .= "$space<script src=\"$_webRoot/libs/myLibs/bower/bootstrap/dist/js/bootstrap.min.js\"></script>".PHP_EOL;
                        break;
                    case 'slicknav':
                        $html .= "$space<script src=\"$_webRoot/libs/myLibs/bower/slicknav/dist/jquery.slicknav.min.js\"></script>".PHP_EOL;
                        break;
                    default:
                        $html .= "$space<script src=\"$_webRoot/libs/js/$item.js\"></script>".PHP_EOL;
                        break;
                }
            }
        }
        if(isset($items['css'])){
            foreach ($items['css'] as $key => $item){
                reset($items['css']);
                if ($key === key($items['css'])){
                    $space = $item;
                    $html .= "$space<!-- myLibs CSS -->".PHP_EOL;
                    continue;
                }
                switch ($item){
                    case 'fonts':
                        $html .= "$space<link rel=\"stylesheet\" type=\"text/css\" href=\"$_webRoot/libs/myLibs/fonts/fonts.css\">\n";
                        break;
                    case 'font-awesome':
                        $html .= "$space<link rel=\"stylesheet\" type=\"text/css\" href=\"$_webRoot/libs/myLibs/bower/font-awesome/css/font-awesome.min.css\">\n";
                        break;
                    case 'bootstrap':
                        $html .= "$space<link rel=\"stylesheet\" type=\"text/css\" href=\"$_webRoot/libs/myLibs/bower/bootstrap/dist/css/bootstrap.min.css\">\n";
                        break;
                    case 'slicknav':
                        $html .= "$space<link rel=\"stylesheet\" type=\"text/css\" href=\"$_webRoot/libs/myLibs/bower/slicknav/dist/slicknav.min.css\">\n";
                        break;
                    case 'animate.css':
                        $html .= "$space<link rel=\"stylesheet\" type=\"text/css\" href=\"$_webRoot/libs/myLibs/bower/animate.css/animate.min.css\">\n";
                        break;
                    default:
                        $html .= "$space<link rel=\"stylesheet\" type=\"text/css\" href=\"$_webRoot/libs/css/$item.css\">\n";
                        break;
                }
            }
        }

        return $html;
    }
    public function dbConnect($credentialArray){
        require_once ($this -> _path.'/bower/PHP-MySQLi-Class/class.database.php');
        return new Database($credentialArray[0],$credentialArray[1],$credentialArray[2],$credentialArray[3]);
    }
    public function httpGetRequest($url){
		return json_decode(file_get_contents($url));
	}
	public function httpPostRequest($url,$data,$header=array()){
		$data = http_build_query($data);
		$ch = curl_init();
		curl_setopt($ch, CURLOPT_URL, $url);
		curl_setopt($ch, CURLOPT_HTTPHEADER, $header);
		curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
		curl_setopt($ch, CURLOPT_POST, 1);
		curl_setopt($ch, CURLOPT_POSTFIELDS, $data);
		$response = curl_exec($ch);
		curl_close($ch);
		return json_decode($response);
	}
	public function cryptoRandSecure($min, $max) {
		$range = $max - $min;
		if ($range < 0) return $min;
		$log = log($range, 2);
		$bytes = (int) ($log / 8) + 1;
		$bits = (int) $log + 1;
		$filter = (int) (1 << $bits) - 1;
		do {
			$rnd = hexdec(bin2hex(openssl_random_pseudo_bytes($bytes)));
			$rnd = $rnd & $filter;
		} while ($rnd >= $range);
		return $min + $rnd;
	}
	public function sendSMS($to,$text){
		$text=rawurlencode($text);
		$options = array(
			'http' => array(
				'header'  => "Content-type: application/x-www-form-urlencoded\r\n",
				'method'  => 'POST',
				'content' => "api_key=04a638c3&api_secret=0c412fd7&from=17022000701&to=$to&text=$text&type=unicode",
			),
		);
		$context  = stream_context_create($options);
		$result = file_get_contents('https://rest.nexmo.com/sms/json', false, $context);
		return json_decode($result) -> messages[0] -> status;
	}
	public function sendSES($to,$subject,$message,$credential,$file = ''){
		require_once($this -> _path.'/aws-sdk-php/vendor/autoload.php');
		$credential = new Aws\Credentials\Credentials($credential[0], $credential[1]);
		if(empty($file)){
			$client = SesClient::factory(array(
				'version' => 'latest',
				'credentials' => $credential,
				'region'  => 'us-west-2'
			));
			$result = true;
			try{
				$client -> sendEmail(array(
					'Source' => "All & IN <no-reply@allandin.com>",
					'Destination' => array(
						'ToAddresses' => array($to),
					),
					'Message' => array(
						'Subject' => array(
							'Data' => $subject,
							'Charset' => 'UTF-8',
						),
						'Body' => array(
							'Text' => array(
								'Data' => $message,
								'Charset' => 'UTF-8',
							),
							'Html' => array(
								'Data' => $message,
								'Charset' => 'UTF-8',
							),
						),
					),
				));

			}
			catch (Exception $e) {
				$result = false;
			}
		}
		else{
			include_once("/var/www/libs/php_class/SESUtils.php");
			$result = SESUtils::sendMail(array(
				"to" => $to,
				"subject" => $subject,
				"message" => $message,
				"from" => "no-reply@letsparty.me",
				"files" => array(
					1 => array(
						"name" => "Invoice.pdf",
						"filepath" => $file,
						"mime" => "application/pdf"
					))
			));
		}
		return $result;
	}
	public function arraySorting($arrays,$sort_key,$sort_order=SORT_ASC,$sort_type=SORT_NUMERIC){
		if(is_array($arrays)){
			foreach ($arrays as $array){
				if(is_array($array)){
					$key_arrays[] = $array[$sort_key];
				}else{
					return false;
				}
			}
		}else{
			return false;
		}
		array_multisort($key_arrays,$sort_order,$sort_type,$arrays);
		return $arrays;
	}
	public function arrayUnique2D($array2D){
		$temp = $res = array();
		foreach ($array2D as $v){
			$v = json_encode($v);
			$temp[] = $v;
		}
		$temp = array_unique($temp);
		foreach ($temp as $item){
			$res[] = json_decode($item,true);
		}
		return $res;
	}
    public function removeElementArray($array,$element){
        $key = array_search($element, $array);
        if ($key !== false)
            array_splice($array, $key, 1);
        return $array;
    }
    public function zhTW2zhCN($article,$reverse=0,$counts=0){
		if (preg_match("/[\x7f-\xff]/", $article)) {
			$rawArticle=$article;
			$zh_TW = '一錒皚藹礙愛翱襖奧壩罷擺敗頒辦絆幫綁鎊謗剝飽寶報鮑輩貝鋇狽備憊繃筆畢斃閉邊編貶變辯辮鱉癟瀕濱賓擯餅撥缽鉑駁蔔補參蠶殘慚慘燦蒼艙倉滄廁側冊測層詫攙摻蟬饞讒纏鏟產闡顫場嘗長償腸廠暢鈔車徹塵陳襯撐稱懲誠騁痴遲馳恥齒熾衝蟲寵疇躊籌綢醜櫥廚鋤雛礎儲觸處傳瘡闖創錘純綽辭詞賜聰蔥囪從叢湊竄錯達帶貸擔單鄲撣膽憚誕彈當擋黨蕩檔搗島禱導盜燈鄧敵滌遞締點墊電澱釣調迭諜疊釘頂錠訂東動棟凍鬥犢獨讀賭鍍鍛斷緞兌隊對噸頓鈍奪鵝額訛惡餓兒爾餌貳發罰閥琺礬釩煩範販飯訪紡飛廢費紛墳奮憤糞豐楓鋒風瘋馮縫諷鳳膚輻撫輔賦復負訃婦縛該鈣蓋干趕稈贛岡剛鋼綱崗皋鎬擱鴿閣鉻個給龔宮鞏貢鉤溝構購夠蠱顧剮關觀館慣貫廣規硅歸龜閨軌詭櫃貴劊輥滾鍋國過駭韓漢閡鶴賀橫轟鴻紅後壺護滬戶嘩華畫劃話懷壞歡環還緩換喚瘓煥渙黃謊揮輝毀賄穢會燴彙諱誨繪葷渾伙獲貨禍擊機積飢譏雞績緝極輯級擠幾薊劑濟計記際繼紀夾莢頰賈鉀價駕殲監堅箋間艱緘繭檢堿鹼揀撿簡儉減薦檻鑒踐賤見鍵艦劍餞漸濺澗漿蔣槳獎講醬膠澆驕嬌攪鉸矯僥腳餃繳絞轎較秸階節莖驚經頸靜鏡徑痙競淨糾廄舊駒舉據鋸懼劇鵑絹傑潔結誡屆緊錦僅謹進晉燼盡勁荊覺決訣絕鈞軍駿開凱顆殼課墾懇摳庫褲誇塊儈寬礦曠況虧巋窺饋潰擴闊蠟腊萊來賴藍欄攔籃闌蘭瀾讕攬覽懶纜爛濫撈勞澇樂鐳壘類淚籬離裡鯉禮麗厲勵礫歷瀝隸倆聯蓮連鐮憐漣簾斂臉鏈戀煉練糧涼兩輛諒療遼鐐獵臨鄰鱗凜賃齡鈴凌靈嶺領餾劉龍聾嚨籠壟攏隴樓婁摟簍蘆盧顱廬爐擄鹵虜魯賂祿錄陸驢呂鋁侶屢縷慮濾綠巒攣孿灤亂掄輪倫侖淪綸論蘿羅邏鑼籮騾駱絡媽瑪碼螞馬罵嗎買麥賣邁脈瞞饅蠻滿謾貓錨鉚貿麼霉沒鎂門悶們錳夢謎彌覓綿緬廟滅憫閩鳴銘謬謀畝鈉納難撓腦惱鬧餒膩攆捻釀鳥聶囓鑷鎳檸獰寧擰濘鈕紐膿濃農瘧諾歐鷗毆嘔漚盤龐國愛賠噴鵬騙飄頻貧蘋憑評潑頗撲鋪樸譜臍齊騎豈啟氣棄訖牽扦钎鉛遷簽謙錢鉗潛淺譴塹槍嗆牆薔強搶鍬橋喬僑翹竅竊欽親輕氫傾頃請慶瓊窮趨區軀驅齲顴權勸卻鵲讓饒擾繞熱韌認紉榮絨軟銳閏潤灑薩鰓賽傘喪騷掃澀殺紗篩曬閃陝贍繕傷賞燒紹賒攝懾設紳審嬸腎滲聲繩勝聖師獅濕詩屍時蝕實識駛勢釋飾視試壽獸樞輸書贖屬術樹豎數帥雙誰稅順說碩爍絲飼聳慫頌訟誦擻蘇訴肅雖綏歲孫損筍縮瑣鎖獺撻抬攤貪癱灘壇譚談嘆湯燙濤絛騰謄銻題體屜條貼鐵廳聽烴銅統頭圖塗團頹蛻脫鴕馱駝橢窪襪彎灣頑萬網韋違圍為濰維葦偉偽緯謂衛溫聞紋穩問甕撾蝸渦窩嗚鎢烏誣無蕪吳塢霧務誤錫犧襲習銑戲細蝦轄峽俠狹廈锨鮮纖鹹賢銜閑顯險現獻縣餡羨憲線廂鑲鄉詳響項蕭銷曉嘯蠍協挾攜脅諧寫瀉謝鋅釁興洶鏽繡虛噓須許緒續軒懸選癬絢學勛詢尋馴訓訊遜壓鴉鴨啞亞訝閹煙鹽嚴顏閻艷厭硯彥諺驗鴦楊揚瘍陽癢養樣瑤搖堯遙窯謠藥爺頁業葉醫銥頤遺儀彝蟻藝億憶義詣議誼譯異繹蔭陰銀飲櫻嬰鷹應纓瑩螢營熒蠅穎喲擁佣癰踊詠湧優憂郵鈾猶游誘輿魚漁娛與嶼語吁御獄譽預馭鴛淵轅園員圓緣遠願約躍鑰岳粵悅閱雲鄖勻隕運蘊醞暈韻雜災載攢暫贊贓髒鑿棗灶責擇則澤賊贈扎札軋鍘閘詐齋債氈盞斬輾嶄棧戰綻張漲帳賬脹趙蟄轍鍺這貞針偵診鎮陣掙睜猙幀鄭證織職執紙摯擲幟質鐘終種腫眾謅軸皺晝驟豬諸誅燭矚囑貯鑄築駐專磚轉賺樁莊裝妝壯狀錐贅墜綴諄濁茲資漬蹤綜總縱鄒詛組鑽致鐘麼為只凶准啟板裡靂余鏈泄';
			$zh_CN = '一锕皑蔼碍爱翱袄奥坝罢摆败颁办绊帮绑镑谤剥饱宝报鲍辈贝钡狈备惫绷笔毕毙闭边编贬变辩辫鳖瘪濒滨宾摈饼拨钵铂驳卜补参蚕残惭惨灿苍舱仓沧厕侧册测层诧搀掺蝉馋谗缠铲产阐颤场尝长偿肠厂畅钞车彻尘陈衬撑称惩诚骋痴迟驰耻齿炽冲虫宠畴踌筹绸丑橱厨锄雏础储触处传疮闯创锤纯绰辞词赐聪葱囱从丛凑窜错达带贷担单郸掸胆惮诞弹当挡党荡档捣岛祷导盗灯邓敌涤递缔点垫电淀钓调迭谍叠钉顶锭订东动栋冻斗犊独读赌镀锻断缎兑队对吨顿钝夺鹅额讹恶饿儿尔饵贰发罚阀珐矾钒烦范贩饭访纺飞废费纷坟奋愤粪丰枫锋风疯冯缝讽凤肤辐抚辅赋复负讣妇缚该钙盖干赶秆赣冈刚钢纲岗皋镐搁鸽阁铬个给龚宫巩贡钩沟构购够蛊顾剐关观馆惯贯广规硅归龟闺轨诡柜贵刽辊滚锅国过骇韩汉阂鹤贺横轰鸿红后壶护沪户哗华画划话怀坏欢环还缓换唤痪焕涣黄谎挥辉毁贿秽会烩汇讳诲绘荤浑伙获货祸击机积饥讥鸡绩缉极辑级挤几蓟剂济计记际继纪夹荚颊贾钾价驾歼监坚笺间艰缄茧检碱硷拣捡简俭减荐槛鉴践贱见键舰剑饯渐溅涧浆蒋桨奖讲酱胶浇骄娇搅铰矫侥脚饺缴绞轿较秸阶节茎惊经颈静镜径痉竞净纠厩旧驹举据锯惧剧鹃绢杰洁结诫届紧锦仅谨进晋烬尽劲荆觉决诀绝钧军骏开凯颗壳课垦恳抠库裤夸块侩宽矿旷况亏岿窥馈溃扩阔蜡腊莱来赖蓝栏拦篮阑兰澜谰揽览懒缆烂滥捞劳涝乐镭垒类泪篱离里鲤礼丽厉励砾历沥隶俩联莲连镰怜涟帘敛脸链恋炼练粮凉两辆谅疗辽镣猎临邻鳞凛赁龄铃凌灵岭领馏刘龙聋咙笼垄拢陇楼娄搂篓芦卢颅庐炉掳卤虏鲁赂禄录陆驴吕铝侣屡缕虑滤绿峦挛孪滦乱抡轮伦仑沦纶论萝罗逻锣箩骡骆络妈玛码蚂马骂吗买麦卖迈脉瞒馒蛮满谩猫锚铆贸么霉没镁门闷们锰梦谜弥觅绵缅庙灭悯闽鸣铭谬谋亩钠纳难挠脑恼闹馁腻撵捻酿鸟聂啮镊镍柠狞宁拧泞钮纽脓浓农疟诺欧鸥殴呕沤盘庞国爱赔喷鹏骗飘频贫苹凭评泼颇扑铺朴谱脐齐骑岂启气弃讫牵扦钎铅迁签谦钱钳潜浅谴堑枪呛墙蔷强抢锹桥乔侨翘窍窃钦亲轻氢倾顷请庆琼穷趋区躯驱龋颧权劝却鹊让饶扰绕热韧认纫荣绒软锐闰润洒萨鳃赛伞丧骚扫涩杀纱筛晒闪陕赡缮伤赏烧绍赊摄慑设绅审婶肾渗声绳胜圣师狮湿诗尸时蚀实识驶势释饰视试寿兽枢输书赎属术树竖数帅双谁税顺说硕烁丝饲耸怂颂讼诵擞苏诉肃虽绥岁孙损笋缩琐锁獭挞抬摊贪瘫滩坛谭谈叹汤烫涛绦腾誊锑题体屉条贴铁厅听烃铜统头图涂团颓蜕脱鸵驮驼椭洼袜弯湾顽万网韦违围为潍维苇伟伪纬谓卫温闻纹稳问瓮挝蜗涡窝呜钨乌诬无芜吴坞雾务误锡牺袭习铣戏细虾辖峡侠狭厦锨鲜纤咸贤衔闲显险现献县馅羡宪线厢镶乡详响项萧销晓啸蝎协挟携胁谐写泻谢锌衅兴汹锈绣虚嘘须许绪续轩悬选癣绚学勋询寻驯训讯逊压鸦鸭哑亚讶阉烟盐严颜阎艳厌砚彦谚验鸯杨扬疡阳痒养样瑶摇尧遥窑谣药爷页业叶医铱颐遗仪彝蚁艺亿忆义诣议谊译异绎荫阴银饮樱婴鹰应缨莹萤营荧蝇颖哟拥佣痈踊咏涌优忧邮铀犹游诱舆鱼渔娱与屿语吁御狱誉预驭鸳渊辕园员圆缘远愿约跃钥岳粤悦阅云郧匀陨运蕴酝晕韵杂灾载攒暂赞赃脏凿枣灶责择则泽贼赠扎札轧铡闸诈斋债毡盏斩辗崭栈战绽张涨帐账胀赵蛰辙锗这贞针侦诊镇阵挣睁狰帧郑证织职执纸挚掷帜质钟终种肿众诌轴皱昼骤猪诸诛烛瞩嘱贮铸筑驻专砖转赚桩庄装妆壮状锥赘坠缀谆浊兹资渍踪综总纵邹诅组钻致钟么为只凶准启板里雳余链泄';
			if($reverse){
				$temp=$zh_TW;
				$zh_TW=$zh_CN;
				$zh_CN=$temp;
			}
			$count = mb_strlen($zh_TW, 'utf-8');
			for($i = 0; $i < $count; $i++){
				$find = mb_substr($zh_TW,$i,1,'utf-8');
				if(strpos($article,$find,0)!==FALSE){
					$replace = mb_substr($zh_CN,$i,1,'utf-8');
					$article = str_replace($find,$replace,$article);
				}
			}
			if(($article==$rawArticle)&&($counts<2)){
				return $this -> zh_TW_zh_CN($article,1,$counts+1);
			}
		}
		return $article;
	}
}
