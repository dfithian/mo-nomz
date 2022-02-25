import * as React from 'react'
import $ from 'jquery'
import Interval from 'react-interval-rerender'
import Layout from '../components/layout'
import { ContentRow } from '../components/row'

class Status extends React.Component {
  constructor(props) {
    super(props)
    this.state = { status: {} }
  }

  componentDidMount() {
      this.Status()
  }

  Status() {
    $.getJSON('/health', (data) => this.setState({ status: data }))
  }

  render() {
    const dateFormatter = (x) => {
      if (x) {
        return new Date(x).toLocaleString()
      } else {
        return "N/A"
      }
    }
    return (
      <div>
        <ContentRow>
          <table className="table">
            <tbody>
              <tr>
                <td>Started</td>
                <td><b>{dateFormatter(this.state.status.started)}</b></td>
              </tr>
              <tr>
                <td>Refreshed</td>
                <td><b>{dateFormatter(this.state.status.fetched)}</b></td>
              </tr>
              <tr>
                <td>Version</td>
                <td><b>{this.state.status.version}</b></td>
              </tr>
              <tr>
                <td>Status</td>
                <td><b>{this.state.status.status}</b></td>
              </tr>
              <tr>
                <td>Users today</td>
                <td><b>{this.state.status.userDay}</b></td>
              </tr>
              <tr>
                <td>Users this week</td>
                <td><b>{this.state.status.userWeek}</b></td>
              </tr>
              <tr>
                <td>Users this month</td>
                <td><b>{this.state.status.userMonth}</b></td>
              </tr>
              <tr>
                <td>Users this year</td>
                <td><b>{this.state.status.userYear}</b></td>
              </tr>
              <tr>
                <td>Cache size</td>
                <td><b>{this.state.status.cacheSize}</b></td>
              </tr>
              <tr>
                <td>Cache most recent</td>
                <td><b>{dateFormatter(this.state.status.cacheMostRecent)}</b></td>
              </tr>
              <tr>
                <td>Cache least recent</td>
                <td><b>{dateFormatter(this.state.status.cacheLeastRecent)}</b></td>
              </tr>
            </tbody>
          </table>
        </ContentRow>
      </div>
    )
  }
}

const StatusPage = () => {
  return (
    <Layout pageTitle="Status">
      <Interval delay={300000}>{() => <Status />}</Interval>
    </Layout>
  )
}

export default StatusPage
