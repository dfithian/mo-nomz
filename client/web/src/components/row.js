import * as React from "react"

const Row = ({ children }) => {
  return (
    <div className="columns is-mobile is-vcentered">
      {children}
    </div>
  )
}

const ContentRow = ({ children }) => {
  return (
    <Row>
      <div className="column is-8 is-offset-2">
        {children}
      </div>
    </Row>
  )
}

export {
  Row,
  ContentRow,
}
