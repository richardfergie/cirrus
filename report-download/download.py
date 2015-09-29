import logging
import StringIO
import sys
import io
import datetime
import os

from googleads import adwords
from googleads import oauth2

logging.basicConfig(level=logging.INFO)
logging.getLogger('suds.transport').setLevel(logging.DEBUG)

# The chunk size used for the report download.
CHUNK_SIZE = 16 * 1024

# Report definitions
campaign_structure_report = {
      'reportName': 'CampaignStructure',
      'dateRangeType' : 'YESTERDAY',
      'reportType': 'CAMPAIGN_PERFORMANCE_REPORT',
      'downloadFormat' : 'CSV',
      'selector': {
          'fields' : ['CampaignId',
                      'CampaignName',
                      'Impressions'
                     ]
      }
  }


adgroup_structure_report = {
      'reportName': 'AdGroupStructure',
      'dateRangeType' : 'YESTERDAY',
      'reportType': 'ADGROUP_PERFORMANCE_REPORT',
      'downloadFormat' : 'CSV',
      'selector': {
          'fields' : ['CampaignId',
                      'AdGroupId',
                      'AdGroupName',
                      'Impressions'
                     ]
      }
  }

textad_structure_report = {
      'reportName': 'TextAdStructure',
      'dateRangeType' : 'YESTERDAY',
      'reportType': 'AD_PERFORMANCE_REPORT',
      'downloadFormat' : 'CSV',
      'selector': {
          'fields' : ['CampaignId',
                      'AdGroupId',
                      'Id',
                      'Headline',
                      'Description1',
                      'Description2',
                      'DisplayUrl',
                      'CreativeDestinationUrl',
                      'Impressions'
                     ],
          'predicates' : [
            {
              'field': 'AdType',
              'operator': 'EQUALS',
              'values': ['TEXT_AD']
              }
          ]
      }
  }

keyword_structure_report = {
      'reportName': 'KeywordStructure',
      'dateRangeType': 'YESTERDAY',
      'reportType': 'KEYWORDS_PERFORMANCE_REPORT',
      'downloadFormat': 'CSV',
      'selector': {
          'fields': ['CampaignId',
                     'AdGroupId',
                     'Id',
                     'Criteria',
                     'KeywordMatchType',
                     'Impressions'
                     ],
        'predicates': [
          {
              'field': 'IsNegative',
              'operator': 'EQUALS',
              'values': ['FALSE']
          }
            ]
          }
      }

structure_reports = [
    campaign_structure_report,
    adgroup_structure_report,
    textad_structure_report,
    keyword_structure_report
                      ]

campaign_attribute_report = {
      'reportName': 'CampaignAttribute',
      'dateRangeType' : 'YESTERDAY',
      'reportType': 'CAMPAIGN_PERFORMANCE_REPORT',
      'downloadFormat' : 'CSV',
      'selector': {
          'fields' : ['CampaignId',
                      'CampaignStatus',
                      'Date',
                      'Impressions'
                     ]
      }
}

adgroup_attribute_report = {
      'reportName': 'AdGroupAttribute',
      'dateRangeType' : 'YESTERDAY',
      'reportType': 'ADGROUP_PERFORMANCE_REPORT',
      'downloadFormat' : 'CSV',
      'selector': {
          'fields' : ['CampaignId',
                      'AdGroupId',
                      'AdGroupStatus',
                      'CpcBid',
                      'Date',
                      'Impressions'
                     ]
      }
  }

textad_attribute_report = {
      'reportName': 'TextAdAttribute',
      'dateRangeType' : 'YESTERDAY',
      'reportType': 'AD_PERFORMANCE_REPORT',
      'downloadFormat' : 'CSV',
      'selector': {
          'fields' : ['CampaignId',
                      'AdGroupId',
                      'Id',
                      'Status',
                      'Date',
                      'Impressions'
                     ],
          'predicates' : [
            {
              'field': 'AdType',
              'operator': 'EQUALS',
              'values': ['TEXT_AD']
              }
          ]
      }
  }

keyword_attribute_report = {
      'reportName': 'KeywordAttribute',
      'dateRangeType': 'YESTERDAY',
      'reportType': 'KEYWORDS_PERFORMANCE_REPORT',
      'downloadFormat': 'CSV',
      'selector': {
          'fields': ['CampaignId',
                     'AdGroupId',
                     'Id',
                     'CpcBid',
                     'QualityScore',
                     'FirstPageCpc',
                     'TopOfPageCpc',
                     'Date',
                     'Impressions'
                     ],
        'predicates': [
          {
              'field': 'IsNegative',
              'operator': 'EQUALS',
              'values': ['FALSE']
          }
            ]
          }
      }

attribute_reports = [
  campaign_attribute_report,
  adgroup_attribute_report,
  textad_attribute_report,
  keyword_attribute_report
]

campaign_performance_report = {
      'reportName': 'CampaignPerformance',
      'dateRangeType' : 'YESTERDAY',
      'reportType': 'CAMPAIGN_PERFORMANCE_REPORT',
      'downloadFormat' : 'CSV',
      'selector': {
          'fields' : ['CampaignId',
                      'Date',
                      'AdNetworkType2',
                      'Clicks',
                      'Impressions',
                      'Cost',
                      'AveragePosition',
                      'ConvertedClicks'
                     ]
      }
}

adgroup_performance_report = {
      'reportName': 'AdGroupPerformance',
      'dateRangeType' : 'YESTERDAY',
      'reportType': 'ADGROUP_PERFORMANCE_REPORT',
      'downloadFormat' : 'CSV',
      'selector': {
          'fields' : ['CampaignId',
                      'AdGroupId',
                      'Date',
                      'AdNetworkType2',
                      'Clicks',
                      'Impressions',
                      'Cost',
                      'AveragePosition',
                      'ConvertedClicks'
                     ]
      }
  }

textad_performance_report = {
      'reportName': 'TextAdPerformance',
      'dateRangeType' : 'YESTERDAY',
      'reportType': 'AD_PERFORMANCE_REPORT',
      'downloadFormat' : 'CSV',
      'selector': {
          'fields' : ['CampaignId',
                      'AdGroupId',
                      'Id',
                      'KeywordId',
                      'Date',
                      'AdNetworkType2',
                      'Clicks',
                      'Impressions',
                      'Cost',
                      'AveragePosition',
                      'ConvertedClicks'
                     ],
          'predicates' : [
            {
              'field': 'AdType',
              'operator': 'EQUALS',
              'values': ['TEXT_AD']
              }
          ]
      }
  }

performance_reports = [
  campaign_performance_report,
  adgroup_performance_report,
  textad_performance_report
]

def downloadReports(reports,zeroimp,client,report_downloader):

  today = datetime.date.today()
  clientstring = client.client_customer_id

  for report in reports:
    reportname = report['reportName']
    filename = '/tmp/' + ':'.join([
                  clientstring,
                  reportname,
                  today.strftime('%Y-%m-%d')
                  ])
    report_data = io.open(filename,'wb')
    stream_data = report_downloader.DownloadReportAsStream(
      report,
      skip_report_header=True,
      skip_column_header=False,
      skip_report_summary=True,
      include_zero_impressions=zeroimp)

    try:
      while True:
        chunk = stream_data.read(CHUNK_SIZE)
        if not chunk: break
        report_data.write(chunk.decode() if sys.version_info[0] == 3
                        and getattr(report_data, 'mode', 'w') == 'w' else chunk)
    finally:
      report_data.close()
      stream_data.close()
    print ("Downloaded "+reportname)

def main(client):
  report_downloader = client.GetReportDownloader(version='v201506')
  downloadReports(structure_reports,True,client,report_downloader)
  downloadReports(attribute_reports,True,client,report_downloader)
  downloadReports(performance_reports,False,client,report_downloader)

if __name__ == '__main__':
  CLIENT_ID = os.environ['ADWORDS_CLIENT_ID']
  CLIENT_SECRET = os.environ['ADWORDS_CLIENT_SECRET']
  REFRESH_TOKEN = os.environ['ADWORDS_REFRESH_TOKEN']
  DEVELOPER_TOKEN = os.environ['ADWORDS_DEVELOPER_TOKEN']
  USER_AGENT = "eanalytica.com-report-downloader"
  CLIENT_CUSTOMER_ID = "929-872-4012"
  oauth2_client = oauth2.GoogleRefreshTokenClient(
      CLIENT_ID, CLIENT_SECRET, REFRESH_TOKEN)
  adwords_client = adwords.AdWordsClient(
      DEVELOPER_TOKEN, oauth2_client, USER_AGENT, CLIENT_CUSTOMER_ID)
  main(adwords_client)
