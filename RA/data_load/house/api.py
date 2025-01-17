import json
import requests
import xmltodict       
import pandas as pd
import key

# load my api key
api_key = key.my_key()


def make_url(lawd_cd, deal_ymd):
    base_url = "http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcSHTrade"
    base_url += f'serviceKey={api_key}'
    base_url += '&pageNo=1&numOfRows=2000'
    base_url += f'&LAWD_CD={lawd_cd}'
    base_url += f'&DEAL_YMD={deal_ymd}'
    return base_url

# request의 결과 행이 1개인 값도 있기 때문
def create_dataframe(data):
    # 데이터가 딕셔너리이고, 값 중 하나라도 리스트인 경우
    if isinstance(data, dict) and any(isinstance(v, list) for v in data.values()):
        df = pd.DataFrame(data)
    # 데이터가 딕셔너리이지만 모든 값이 스칼라인 경우
    elif isinstance(data, dict):
        df = pd.DataFrame([data])
    # 데이터가 리스트이면 바로 DataFrame 생성
    elif isinstance(data, list):
        df = pd.DataFrame(data)
    else:
        raise ValueError("Unsupported data type")
    
    return df

def get_df(lawd_cd, deal_ymd):
    import requests
    import json
    import pandas as pd
    import xmltodict
    base_url= make_url(lawd_cd, deal_ymd)
    
    try:
        res = requests.get(base_url)
        data = json.loads(json.dumps(xmltodict.parse(res.text)))
        result_code = data.get('response', {}).get('header').get('resultCode')
        #if result_code == '99':
        #    msg = data.get('response', {}).get('header').get('resultMsg')
        #   return msg
        #else :
        # items가 None이거나 'item' 키가 없는 경우를 처리
        items = data.get('response', {}).get('body', {}).get('items', None)
        if items is None or 'item' not in items:
            # print(f"No data available or incorrect data structure on ({lawd_cd}, {deal_ymd})")
            return None
        
        df = create_dataframe(items['item'])
        return df

    except KeyError as e:
        print(f"KeyError - reason: {str(e)} on ({lawd_cd}, {deal_ymd})")
        return None
    except TypeError as e:
        print(f"TypeError - reason: {str(e)} on ({lawd_cd}, {deal_ymd})")
        return None
    except Exception as e:
        print(f"An error occurred: {str(e)} on ({lawd_cd}, {deal_ymd})")
        return None

# 데이터 저장하기 
def save_data(engine, tab_name, loc_code, year_month):
    df = get_df(loc_code, year_month)
    if df is not None:
        df.to_sql(f'{tab_name}', con=engine, if_exists='append', index=False)
        return True
    #elif : df == 
    else : 
        return False


# 지역별, 월별 생성하기

def save_all(engine, tab_name, loc_list, month_list):
    for month in month_list :
        no_data_count = 0 
        for code in loc_list:
            result = save_data(engine, tab_name, code, month)
            if not result :
                no_data_count += 1
        print(f"{month} data is saved. {no_data_count} locations with no data saved.")


# 지역구 코드 생성기
def loc_code(loc = "all"):
    loc_dict = {"seoul": 1, "all" : "all"}
    import pandas as pd
    data = pd.read_csv('법정동코드 전체자료.txt', sep='\t', encoding='cp949')
    loc_code_total = data['법정동코드'].astype(str).str[:5]
    all_code = loc_code_total.unique()

    if loc_dict[loc] == 'all' :
        code = [int(i) for i in all_code]
    else:
        code = [int(i) for i in all_code if i.startswith(str(loc_dict[loc]))]
    
    filtered_code = [num for num in code if num % 1000 != 0]

    return filtered_code



# 연월 코드 생성기
def create_ym_list(start_year, end_year):
    year = list(range(start_year, end_year+1))
    month = list(range(1,13))
    ym_list =[]
    for y in year:
        for m in month:
            ym = f"{y}{m:02d}"
            ym_list.append(ym)

    return ym_list
# create_ym_list(2022,2023)


def debug_request(loc_code, ym_code):
    base_url= make_url(loc_code, ym_code)
    res = requests.get(base_url)
    data = json.loads(json.dumps(xmltodict.parse(res.text)))
    result_code = data.get('response', {}).get('header').get('resultCode')
    items = data.get('response', {}).get('body', {}).get('items', None)
    return data, result_code, items